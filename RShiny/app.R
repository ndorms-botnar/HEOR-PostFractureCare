#### PACKAGES -----
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(kableExtra)
library(shinycssloaders)
library(tidyr)
library(stringr)
library(scales)
library(DT)
library(rms)
library(broom)
library(purrr)

library(here)


#library(snow)
#library(parallel)

# N microsimulation ----

n_microsimulation<-75000#500#350#
 # will use this for each site and sex
 # eg 1000 hip fx male, 1000 spine fx male, etc
# global ----
source('global.R', local = TRUE)

#### Functions ------

# load functions from functions folder
files<-list.files("functions")
for(i in 1:length(files)){
source(paste0("functions/",files[i]), local = TRUE)
}


gg_facet_nrow<- function(p){
  meds.plot %>% ggplot2::ggplot_build()  %>%
  magrittr::extract2('layout')       %>%
  magrittr::extract2('panel_layout') %>%
  magrittr::extract2('ROW')          %>%
  unique()                           %>%
  length()
  }
#### UI -----
ui <-  fluidPage(
 # theme = "yeti.css",
  theme = shinytheme("yeti"),
           useShinyjs(),  # Include shinyjs  


# title ------ 
# shown across tabs
titlePanel("FLS Benefit and Budget Impact Calculator"),

                 
# set up: pages along the side -----  
                 navlistPanel( 
            
# Introduction pages -----  
"Background",
tabPanel("Overview", 
    mainPanel(
      tags$script(HTML(
        "var socket_timeout_interval;
var n = 0;

$(document).on('shiny:connected', function(event) {
  socket_timeout_interval = setInterval(function() {
    Shiny.onInputChange('alive_count', n++)
  }, 10000);
});

$(document).on('shiny:disconnected', function(event) {
  clearInterval(socket_timeout_interval)
});"
      )),
      textOutput("keep_alive"),
      tags$h3("Overview"),
      tags$hr(),
      tags$h4(uiOutput("intro_text")),
      tags$hr(), 
      tags$h3("Model outline"),
      tags$img(src = "model outline.png"),
      tags$hr()
    )),   
# Model inputs pages ------ 
"Model inputs",
## Setting ----
tabPanel("Setting", 
    mainPanel(
      tags$h3("Setting"),
      tags$hr(),
      tags$h4(uiOutput("setting_text")),
      tags$hr(),
      tags$h5(uiOutput("country.name")), #user to input, no default value
      tags$h5(uiOutput("region.selection")),  #user to input, no default value
      tags$hr()
      )),
# Index fractures ----
tabPanel("Sentinel fractures", 
    mainPanel(
      tags$h3("Sentinel fractures"),
      tags$hr(),
      tags$h4(uiOutput("index_fx_general_text")),
      tags$hr(),

      splitLayout(
        tags$h4("1) Index fractures: all"),
         prettyCheckbox(inputId = "checkbox_index_fx_option_1",
                 label = "",
                 shape = "round",
                 status = "danger",
                 value = TRUE),
      cellWidths = c("85%", "15%")),
      tags$h5(tags$em(uiOutput("index_fx_option_1_text"))),
       a(id = "toggle.specify_all_fx",
        tags$h5("View/ edit")),
shinyjs::hidden(
tags$div(id = 'specify_all_fx',
         tags$h5("Number of spine fractures: Males",
      numericInput('all_spine_fx_n.male', '', 
                value="", step=50)), #user to input, no default value
      tags$h5("Number of hip fractures: Males",
      numericInput('all_hip_fx_n.male', '', 
                value="", step=50)), #user to input, no default value
      tags$h5("Number of other fractures: Males",
      numericInput('all_other_fx_n.male', '', 
                value="", step=50)), #user to input, no default value
      
            tags$h5("Number of spine fractures: Females",
      numericInput('all_spine_fx_n.female', '', 
                value="", step=50)), #user to input, no default value
      tags$h5("Number of hip fractures: Females",
      numericInput('all_hip_fx_n.female', '', 
                value="", step=50)), #user to input, no default value
      tags$h5("Number of other fractures: Females",
      numericInput('all_other_fx_n.female', '', 
                value="", step=50)) #user to input, no default value
)),
 splitLayout(
        tags$h4("2) Index fractures: hip only"),
         prettyCheckbox(inputId = "checkbox_index_fx_option_2",
                 label = "",
                 shape = "round",
                 status = "danger",
                 value = FALSE),
      cellWidths = c("85%", "15%")),  
      tags$h5(tags$em(uiOutput("index_fx_option_2_text"))),
       a(id = "toggle.specify_hip_fx_only",
        tags$h6("View/ edit")),
shinyjs::hidden(
tags$div(id = 'specify_hip_fx_only',
      tags$h5("Number of hip fractures: Males",
      numericInput('only_hip_fx_n.male', '', 
                value="", step=50)), #user to input, no default value
        tags$h5("Number of hip fractures: Females",
      numericInput('only_hip_fx_n.female', '', 
                value="", step=50)) #user to input, no default value
)),
     
 splitLayout(
        tags$h4("3) General population"),
         prettyCheckbox(inputId = "checkbox_index_fx_option_3",
                 label = "",
                 shape = "round",
                 status = "danger",
                 value = FALSE),
      cellWidths = c("85%", "15%")),
      tags$h5(tags$em(uiOutput("index_fx_option_3_text"))),
       a(id = "toggle.specify_pop_szie",
        tags$h6("View/ edit")),
shinyjs::hidden(
tags$div(id = 'specify_pop_size',
      tags$h5("Population size",
      uiOutput("pop_size")       
              
      # numericInput('pop_size', '', 
      #           value="", step=50)
      ), #user to input, no default value
      
      
textOutput("label.gen_pop_over_50"),
uiOutput("gen_pop_over_50"), #default, but user can input
# textOutput("label.over_50.female"),
# uiOutput("over_50.female"), #default, but user can input
textOutput("label.prob_hip_fx_over_50.male"),
uiOutput("prob_hip_fx_over_50.male"), #default, but user can input
textOutput("label.prob_hip_fx_over_50.female"),
uiOutput("prob_hip_fx_over_50.female") #default, but user can input


  )),
tags$hr()
)),
# study population -----
tabPanel("Study population", 
    mainPanel( 
        tags$h3("Study population: age at fracture"),
      tags$hr(),  
      tags$h4(uiOutput("study_pop_general_text")),
 #     tags$hr(),
# textOutput("label.prop.female"),
# uiOutput("prop.female"), #default, but user can input    
tags$hr(),
tags$h5(tags$em(uiOutput("study_pop_age_text"))),
a(id = "toggle.specify_pop_age",
        tags$h5("View/ edit")),
shinyjs::hidden(
tags$div(id = 'specify_pop_age',
textOutput("label.av_age_hip.male"),
uiOutput("av_age_hip.male"), #default, but user can input
textOutput("label.av_age_spine.male"),
uiOutput("av_age_spine.male"), #default, but user can input
textOutput("label.av_age_other.male"),
uiOutput("av_age_other.male"), #default, but user can input

textOutput("label.av_age_hip.female"),
uiOutput("av_age_hip.female"), #default, but user can input
textOutput("label.av_age_spine.female"),
uiOutput("av_age_spine.female"), #default, but user can input
textOutput("label.av_age_other.female"),
uiOutput("av_age_other.female") #default, but user can input

))
    )),  

## Backgound risks of refracture ----        
tabPanel("Risk of refracture",
       mainPanel(
         
         tags$h3("Risk of refracture"),
         tags$hr(),
         tags$h4(uiOutput("backgound_ risks_refrac_text")),
         tags$hr(),
            splitLayout(
        tags$h4("1) 5-year risks of refracture"),
         prettyCheckbox(inputId = "checkbox_index_refx_5y",
                 label = "",
                 shape = "round",
                 status = "danger",
                 value = TRUE),
      cellWidths = c("85%", "15%")),
      tags$h5(tags$em(uiOutput("index_refx_5y_text"))),
          a(id = "toggle.specify.5y_refracture",
        tags$h6("View/ edit")),
         shinyjs::hidden(

tags$div(id = 'specify.5y_refracture',
      tags$h4("After hip fracture"),
      textOutput("label.risk_hip_fracture_after_hip_5y.male"),
      tags$h5(uiOutput("risk_hip_fracture_after_hip_5y.male")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_hip_5y.male"),
      tags$h5(uiOutput("risk_spine_fracture_after_hip_5y.male")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_hip_5y.male"),
      tags$h5(uiOutput("risk_other_fracture_after_hip_5y.male")), #user to input, no default value

      textOutput("label.risk_hip_fracture_after_hip_5y.female"),
      tags$h5(uiOutput("risk_hip_fracture_after_hip_5y.female")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_hip_5y.female"),
      tags$h5(uiOutput("risk_spine_fracture_after_hip_5y.female")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_hip_5y.female"),
      tags$h5(uiOutput("risk_other_fracture_after_hip_5y.female")), #user to input, no default value

      tags$h4("After spine fracture"),
      textOutput("label.risk_hip_fracture_after_spine_5y.male"),
      tags$h5(uiOutput("risk_hip_fracture_after_spine_5y.male")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_spine_5y.male"),
      tags$h5(uiOutput("risk_spine_fracture_after_spine_5y.male")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_spine_5y.male"),
      tags$h5(uiOutput("risk_other_fracture_after_spine_5y.male")), #user to input, no default value
 
      
      textOutput("label.risk_hip_fracture_after_spine_5y.female"),
      tags$h5(uiOutput("risk_hip_fracture_after_spine_5y.female")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_spine_5y.female"),
      tags$h5(uiOutput("risk_spine_fracture_after_spine_5y.female")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_spine_5y.female"),
      tags$h5(uiOutput("risk_other_fracture_after_spine_5y.female")), #user to input, no default value

      
       tags$h4("After other fracture"),
      textOutput("label.risk_hip_fracture_after_other_5y.male"),
      tags$h5(uiOutput("risk_hip_fracture_after_other_5y.male")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_other_5y.male"),
      tags$h5(uiOutput("risk_spine_fracture_after_other_5y.male")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_other_5y.male"),
      tags$h5(uiOutput("risk_other_fracture_after_other_5y.male")), #user to input, no default value
 
      textOutput("label.risk_hip_fracture_after_other_5y.female"),
      tags$h5(uiOutput("risk_hip_fracture_after_other_5y.female")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_other_5y.female"),
      tags$h5(uiOutput("risk_spine_fracture_after_other_5y.female")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_other_5y.female"),
      tags$h5(uiOutput("risk_other_fracture_after_other_5y.female")) #user to input, no default value

)),

     
            splitLayout(
        tags$h4("2) 10-year risks of refracture"),
         prettyCheckbox(inputId = "checkbox_index_refx_10y",
                 label = "",
                 shape = "round",
                 status = "danger",
                 value = FALSE),
      cellWidths = c("85%", "15%")),
      tags$h5(tags$em(uiOutput("index_refx_10y_text"))),                 

                     a(id = "toggle.specify.10y_refracture",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'specify.10y_refracture',
                 
        tags$h4("After hip fracture"),
            textOutput("label.risk_hip_fracture_after_hip_10y.male"),
      tags$h5(uiOutput("risk_hip_fracture_after_hip_10y.male")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_hip_10y.male"),
      tags$h5(uiOutput("risk_spine_fracture_after_hip_10y.male")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_hip_10y.male"),
      tags$h5(uiOutput("risk_other_fracture_after_hip_10y.male")), #user to input, no default value

                  textOutput("label.risk_hip_fracture_after_hip_10y.female"),
      tags$h5(uiOutput("risk_hip_fracture_after_hip_10y.female")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_hip_10y.female"),
      tags$h5(uiOutput("risk_spine_fracture_after_hip_10y.female")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_hip_10y.female"),
      tags$h5(uiOutput("risk_other_fracture_after_hip_10y.female")), #user to input, no default value
      
        tags$h4("After spine fracture"),
      textOutput("label.risk_hip_fracture_after_spine_10y.male"),
      tags$h5(uiOutput("risk_hip_fracture_after_spine_10y.male")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_spine_10y.male"),
      tags$h5(uiOutput("risk_spine_fracture_after_spine_10y.male")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_spine_10y.male"),
      tags$h5(uiOutput("risk_other_fracture_after_spine_10y.male")), #user to input, no default value
 
      
      textOutput("label.risk_hip_fracture_after_spine_10y.female"),
      tags$h5(uiOutput("risk_hip_fracture_after_spine_10y.female")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_spine_10y.female"),
      tags$h5(uiOutput("risk_spine_fracture_after_spine_10y.female")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_spine_10y.female"),
      tags$h5(uiOutput("risk_other_fracture_after_spine_10y.female")), #user to input, no default value

      
      
        tags$h4("After other fracture"),
      textOutput("label.risk_hip_fracture_after_other_10y.male"),
      tags$h5(uiOutput("risk_hip_fracture_after_other_10y.male")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_other_10y.male"),
      tags$h5(uiOutput("risk_spine_fracture_after_other_10y.male")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_other_10y.male"),
      tags$h5(uiOutput("risk_other_fracture_after_other_10y.male")), #user to input, no default value

    
      textOutput("label.risk_hip_fracture_after_other_10y.female"),
      tags$h5(uiOutput("risk_hip_fracture_after_other_10y.female")), #user to input, no default value
      textOutput("label.risk_spine_fracture_after_other_10y.female"),
      tags$h5(uiOutput("risk_spine_fracture_after_other_10y.female")), #user to input, no default value
      textOutput("label.risk_other_fracture_after_other_10y.female"),
      tags$h5(uiOutput("risk_other_fracture_after_other_10y.female")) #user to input, no default value

))

      )),


## Tiered re-fracture risk ----
tabPanel("Risk profiles",
    tabsetPanel(type = "tabs",
          tabPanel("Risk profiles",
          tabsetPanel(type = "pills",
     tabPanel("Tiered re-fracture risk",
       mainPanel( 
         tags$hr(),
         textOutput("label.prop.fx.low.risk"),
         tags$h4(uiOutput("prop.fx.low.risk")),
         textOutput("label.prop.fx.intermediate.risk"),
         tags$h4(uiOutput("prop.fx.intermediate.risk")),
         textOutput("label.prop.fx.high.risk"),
         tags$h4(uiOutput("prop.fx.high.risk")))),
          tabPanel("Re-fracture risk multiplier by risk tier",
       mainPanel( 
         tags$hr(),
         textOutput("label.fx.multiplier.low.risk"),
         tags$h4(uiOutput("fx.multiplier.low.risk")),
         textOutput("label.fx.multiplier.intermediate.risk"),
         tags$h4(uiOutput("fx.multiplier.intermediate.risk")),
         textOutput("label.fx.multiplier.high.risk"),
         tags$h4(uiOutput("fx.multiplier.high.risk"))))
          )))), 
## Backgound risks of death ----        
tabPanel("Risk of death", 
    mainPanel(
               tags$h3("Risk of death following fracture"),
         tags$hr(),
         tags$h4(uiOutput("backgound_risks_death_text")),
         tags$hr(),
               tags$h5(tags$em(uiOutput("risk_death_text"))),                 
         

                   a(id = "toggle.specify.death",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'specify.death',

        tags$h4("After first hip fracture"),
            textOutput("label.prob.death_hf1_0_3.male"),
      tags$h5(uiOutput("prob.death_hf1_0_3.male")), 
            textOutput("label.prob.death_hf1_0_12.male"),
      tags$h5(uiOutput("prob.death_hf1_0_12.male")), 
            textOutput("label.prob.death_hf1_0_60.male"),
      tags$h5(uiOutput("prob.death_hf1_0_60.male")), 
      
            textOutput("label.prob.death_hf1_0_3.female"),
      tags$h5(uiOutput("prob.death_hf1_0_3.female")), 
            textOutput("label.prob.death_hf1_0_12.female"),
      tags$h5(uiOutput("prob.death_hf1_0_12.female")), 
            textOutput("label.prob.death_hf1_0_60.female"),
      tags$h5(uiOutput("prob.death_hf1_0_60.female")), 


      tags$h4("After second hip fracture"),
           textOutput("label.prob.death_hf2_0_3.male"),
      tags$h5(uiOutput("prob.death_hf2_0_3.male")), 
            textOutput("label.prob.death_hf2_0_12.male"),
      tags$h5(uiOutput("prob.death_hf2_0_12.male")), 
            textOutput("label.prob.death_hf2_0_60.male"),
      tags$h5(uiOutput("prob.death_hf2_0_60.male")),
      
                 textOutput("label.prob.death_hf2_0_3.female"),
      tags$h5(uiOutput("prob.death_hf2_0_3.female")), 
            textOutput("label.prob.death_hf2_0_12.female"),
      tags$h5(uiOutput("prob.death_hf2_0_12.female")), 
            textOutput("label.prob.death_hf2_0_60.female"),
      tags$h5(uiOutput("prob.death_hf2_0_60.female")),
      
      
       tags$h4("After a spine fracture"),
          textOutput("label.prob.death_sf_0_3.male"),
      tags$h5(uiOutput("prob.death_sf_0_3.male")), 
            textOutput("label.prob.death_sf_0_12.male"),
      tags$h5(uiOutput("prob.death_sf_0_12.male")), 
            textOutput("label.prob.death_sf_0_60.male"),
      tags$h5(uiOutput("prob.death_sf_0_60.male")), 
      
                  textOutput("label.prob.death_sf_0_3.female"),
      tags$h5(uiOutput("prob.death_sf_0_3.female")), 
            textOutput("label.prob.death_sf_0_12.female"),
      tags$h5(uiOutput("prob.death_sf_0_12.female")), 
            textOutput("label.prob.death_sf_0_60.female"),
      tags$h5(uiOutput("prob.death_sf_0_60.female"))
         ))

)),
## Qol----        
tabPanel("Qol", 
    mainPanel(
        tags$h4("Hip fracture"),
            textOutput("label.qol.pre.hip"),
      tags$h5(uiOutput("qol.pre.hip")), 
            textOutput("label.qol.post.hip"),
      tags$h5(uiOutput("qol.post.hip")), 
            textOutput("label.qol.post.4.month.hip"),
      tags$h5(uiOutput("qol.post.4.month.hip")), 
      
        tags$h4("spine fracture"),
            textOutput("label.qol.pre.spine"),
      tags$h5(uiOutput("qol.pre.spine")), 
            textOutput("label.qol.post.spine"),
      tags$h5(uiOutput("qol.post.spine")), 
            textOutput("label.qol.post.4.month.spine"),
      tags$h5(uiOutput("qol.post.4.month.spine")),       
      
        tags$h4("Other fracture"),
            textOutput("label.qol.pre.other"),
      tags$h5(uiOutput("qol.pre.other")), 
            textOutput("label.qol.post.other"),
      tags$h5(uiOutput("qol.post.other")), 
            textOutput("label.qol.post.4.month.other"),
      tags$h5(uiOutput("qol.post.4.month.other"))
         

)),

## Comparators -----
tabPanel("Comparators",
    tabsetPanel(type = "tabs",
          tabPanel("Probability of being identified",
          tabsetPanel(type = "pills",
     tabPanel("Current practice",
       mainPanel(    
         tags$h3("Probability of being identified"),
         tags$hr(),
         tags$h4(uiOutput("prob_identified_no_fls_text")),
         tags$hr(),
                            a(id = "toggle.iden_no_fls_text",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'iden_no_fls_text',

           textOutput("label.no.fls.prob_identification.hip"),
      tags$h5(uiOutput("no.fls.prob_identification.hip")),
                 textOutput("label.no.fls.prob_identification.spine"),
      tags$h5(uiOutput("no.fls.prob_identification.spine")),
 textOutput("label.no.fls.prob_identification.other"),
      tags$h5(uiOutput("no.fls.prob_identification.other"))
))
       )),
    tabPanel("FLS",
       mainPanel(   
                      tags$h3("Probability of being identified"),
                    tags$hr(),
             tags$h4(uiOutput("prob_identified_fls_text")),
         tags$hr(),
           a(id = "toggle.iden_fls_text",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'iden_fls_text',
           textOutput("label.fls.prob_identification.hip"),
      tags$h5(uiOutput("fls.prob_identification.hip")),
                 textOutput("label.fls.prob_identification.spine"),
      tags$h5(uiOutput("fls.prob_identification.spine")),
 textOutput("label.fls.prob_identification.other"),
      tags$h5(uiOutput("fls.prob_identification.other")),   
         tags$hr()
       ))
       ))
 )),
 
  tabPanel("Time to treatment onset",
       tabsetPanel(type = "pills",
     tabPanel("Current practice",
             mainPanel(    
             tags$h3("Time to treatment onset"),
             tags$hr(),
         tags$h4(uiOutput("prob_time_treatment_no_fls_text")),
         tags$hr(),
          a(id = "toggle.time_no_fls_text",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'time_no_fls_text',

            textOutput("label.no.fls.time_to_treat.hip"),
      tags$h5(uiOutput("no.fls.time_to_treat.hip")),
            textOutput("label.no.fls.time_to_treat.spine"),
      tags$h5(uiOutput("no.fls.time_to_treat.spine")),
            textOutput("label.no.fls.time_to_treat.other"),
      tags$h5(uiOutput("no.fls.time_to_treat.other"))
           ))
      )),
     tabPanel("FLS",
            mainPanel(       
            tags$h3("Time to treatment onset"),
             tags$hr(),
           tags$h4(uiOutput("prob_time_treatment_fls_text")),
         tags$hr(),
                   a(id = "toggle.time_fls_text",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'time_fls_text',

           textOutput("label.fls.time_to_treat.hip"),
      tags$h5(uiOutput("fls.time_to_treat.hip")),
            textOutput("label.fls.time_to_treat.spine"),
      tags$h5(uiOutput("fls.time_to_treat.spine")),
            textOutput("label.fls.time_to_treat.other"),
      tags$h5(uiOutput("fls.time_to_treat.other"))
           ))
     ))
       )),
 
  tabPanel("Medications after sentinel fracture",
       tabsetPanel(type = "pills",
     tabPanel("Current practice",
             mainPanel(    
                    tags$h3("Medications after sentinel fracture"),
             tags$hr(),
          tags$h4(uiOutput("med_no_fls_text")),
         tags$hr(),
                    a(id = "toggle.meds_no_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'meds_no_fls',
                    
   tags$h4("After hip fracture"),
           textOutput("label.no.fls.trt.hip.male"),
      tags$h5(uiOutput("no.fls.trt.hip.male")),
           textOutput("label.no.fls.trt.alendronate.hip.male"),
      tags$h5(uiOutput("no.fls.trt.alendronate.hip.male")),
      textOutput("label.no.fls.trt.risedronate.hip.male"),
      tags$h5(uiOutput("no.fls.trt.risedronate.hip.male")),
       textOutput("label.no.fls.trt.strontium.hip.male"),
      tags$h5(uiOutput("no.fls.trt.strontium.hip.male")),
           textOutput("label.no.fls.trt.ibandronate.hip.male"),
      tags$h5(uiOutput("no.fls.trt.ibandronate.hip.male")),
              textOutput("label.no.fls.trt.raloxifene.hip.male"),
      tags$h5(uiOutput("no.fls.trt.raloxifene.hip.male")),
           textOutput("label.no.fls.trt.denosumab.hip.male"),
      tags$h5(uiOutput("no.fls.trt.denosumab.hip.male")),
           textOutput("label.no.fls.trt.zoledronate.hip.male"),
      tags$h5(uiOutput("no.fls.trt.zoledronate.hip.male")),
            textOutput("label.no.fls.trt.teriparatide.hip.male"),
      tags$h5(uiOutput("no.fls.trt.teriparatide.hip.male")),
           textOutput("label.no.fls.trt.abaloparatide.hip.male"),
      tags$h5(uiOutput("no.fls.trt.abaloparatide.hip.male")),
           textOutput("label.no.fls.trt.romo.hip.male"),
      tags$h5(uiOutput("no.fls.trt.romo.hip.male")),
   
              textOutput("label.no.fls.trt.hip.female"),
      tags$h5(uiOutput("no.fls.trt.hip.female")),
           textOutput("label.no.fls.trt.alendronate.hip.female"),
      tags$h5(uiOutput("no.fls.trt.alendronate.hip.female")),
      textOutput("label.no.fls.trt.risedronate.hip.female"),
      tags$h5(uiOutput("no.fls.trt.risedronate.hip.female")),
       textOutput("label.no.fls.trt.strontium.hip.female"),
      tags$h5(uiOutput("no.fls.trt.strontium.hip.female")),
           textOutput("label.no.fls.trt.ibandronate.hip.female"),
      tags$h5(uiOutput("no.fls.trt.ibandronate.hip.female")),
           textOutput("label.no.fls.trt.raloxifene.hip.female"),
      tags$h5(uiOutput("no.fls.trt.raloxifene.hip.female")),
           textOutput("label.no.fls.trt.denosumab.hip.female"),
      tags$h5(uiOutput("no.fls.trt.denosumab.hip.female")),
           textOutput("label.no.fls.trt.zoledronate.hip.female"),
      tags$h5(uiOutput("no.fls.trt.zoledronate.hip.female")),
            textOutput("label.no.fls.trt.teriparatide.hip.female"),
      tags$h5(uiOutput("no.fls.trt.teriparatide.hip.female")),
           textOutput("label.no.fls.trt.abaloparatide.hip.female"),
      tags$h5(uiOutput("no.fls.trt.abaloparatide.hip.female")),
           textOutput("label.no.fls.trt.romo.hip.female"),
      tags$h5(uiOutput("no.fls.trt.romo.hip.female")),


      tags$h4("After spine fracture"),
            textOutput("label.no.fls.trt.spine.male"),
      tags$h5(uiOutput("no.fls.trt.spine.male")),
           textOutput("label.no.fls.trt.alendronate.spine.male"),
      tags$h5(uiOutput("no.fls.trt.alendronate.spine.male")),
      textOutput("label.no.fls.trt.risedronate.spine.male"),
      tags$h5(uiOutput("no.fls.trt.risedronate.spine.male")),
       textOutput("label.no.fls.trt.strontium.spine.male"),
      tags$h5(uiOutput("no.fls.trt.strontium.spine.male")),
           textOutput("label.no.fls.trt.ibandronate.spine.male"),
      tags$h5(uiOutput("no.fls.trt.ibandronate.spine.male")),
           textOutput("label.no.fls.trt.raloxifene.spine.male"),
      tags$h5(uiOutput("no.fls.trt.raloxifene.spine.male")),
           textOutput("label.no.fls.trt.denosumab.spine.male"),
      tags$h5(uiOutput("no.fls.trt.denosumab.spine.male")),
           textOutput("label.no.fls.trt.zoledronate.spine.male"),
      tags$h5(uiOutput("no.fls.trt.zoledronate.spine.male")),
            textOutput("label.no.fls.trt.teriparatide.spine.male"),
      tags$h5(uiOutput("no.fls.trt.teriparatide.spine.male")),
           textOutput("label.no.fls.trt.abaloparatide.spine.male"),
      tags$h5(uiOutput("no.fls.trt.abaloparatide.spine.male")),
           textOutput("label.no.fls.trt.romo.spine.male"),
      tags$h5(uiOutput("no.fls.trt.romo.spine.male")),
 
  
            textOutput("label.no.fls.trt.spine.female"),
      tags$h5(uiOutput("no.fls.trt.spine.female")),
           textOutput("label.no.fls.trt.alendronate.spine.female"),
      tags$h5(uiOutput("no.fls.trt.alendronate.spine.female")),
      textOutput("label.no.fls.trt.risedronate.spine.female"),
      tags$h5(uiOutput("no.fls.trt.risedronate.spine.female")),
       textOutput("label.no.fls.trt.strontium.spine.female"),
      tags$h5(uiOutput("no.fls.trt.strontium.spine.female")),
           textOutput("label.no.fls.trt.ibandronate.spine.female"),
      tags$h5(uiOutput("no.fls.trt.ibandronate.spine.female")),
           textOutput("label.no.fls.trt.raloxifene.spine.female"),
      tags$h5(uiOutput("no.fls.trt.raloxifene.spine.female")),
           textOutput("label.no.fls.trt.denosumab.spine.female"),
      tags$h5(uiOutput("no.fls.trt.denosumab.spine.female")),
           textOutput("label.no.fls.trt.zoledronate.spine.female"),
      tags$h5(uiOutput("no.fls.trt.zoledronate.spine.female")),
            textOutput("label.no.fls.trt.teriparatide.spine.female"),
      tags$h5(uiOutput("no.fls.trt.teriparatide.spine.female")),
           textOutput("label.no.fls.trt.abaloparatide.spine.female"),
      tags$h5(uiOutput("no.fls.trt.abaloparatide.spine.female")),
           textOutput("label.no.fls.trt.romo.spine.female"),
      tags$h5(uiOutput("no.fls.trt.romo.spine.female")),
 
   
   
      tags$h4("After other fracture"),
             tags$hr(),
            textOutput("label.no.fls.trt.other.male"),
      tags$h5(uiOutput("no.fls.trt.other.male")),
           textOutput("label.no.fls.trt.alendronate.other.male"),
      tags$h5(uiOutput("no.fls.trt.alendronate.other.male")),
      textOutput("label.no.fls.trt.risedronate.other.male"),
      tags$h5(uiOutput("no.fls.trt.risedronate.other.male")),
       textOutput("label.no.fls.trt.strontium.other.male"),
      tags$h5(uiOutput("no.fls.trt.strontium.other.male")),
           textOutput("label.no.fls.trt.ibandronate.other.male"),
      tags$h5(uiOutput("no.fls.trt.ibandronate.other.male")),
           textOutput("label.no.fls.trt.raloxifene.other.male"),
      tags$h5(uiOutput("no.fls.trt.raloxifene.other.male")),
           textOutput("label.no.fls.trt.denosumab.other.male"),
      tags$h5(uiOutput("no.fls.trt.denosumab.other.male")),
           textOutput("label.no.fls.trt.zoledronate.other.male"),
      tags$h5(uiOutput("no.fls.trt.zoledronate.other.male")),
            textOutput("label.no.fls.trt.teriparatide.other.male"),
      tags$h5(uiOutput("no.fls.trt.teriparatide.other.male")),
           textOutput("label.no.fls.trt.abaloparatide.other.male"),
      tags$h5(uiOutput("no.fls.trt.abaloparatide.other.male")),
           textOutput("label.no.fls.trt.romo.other.male"),
      tags$h5(uiOutput("no.fls.trt.romo.other.male")),
 


            textOutput("label.no.fls.trt.other.female"),
      tags$h5(uiOutput("no.fls.trt.other.female")),
           textOutput("label.no.fls.trt.alendronate.other.female"),
      tags$h5(uiOutput("no.fls.trt.alendronate.other.female")),
      textOutput("label.no.fls.trt.risedronate.other.female"),
      tags$h5(uiOutput("no.fls.trt.risedronate.other.female")),
       textOutput("label.no.fls.trt.strontium.other.female"),
      tags$h5(uiOutput("no.fls.trt.strontium.other.female")),
           textOutput("label.no.fls.trt.ibandronate.other.female"),
      tags$h5(uiOutput("no.fls.trt.ibandronate.other.female")),
           textOutput("label.no.fls.trt.raloxifene.other.female"),
      tags$h5(uiOutput("no.fls.trt.raloxifene.other.female")),
           textOutput("label.no.fls.trt.denosumab.other.female"),
      tags$h5(uiOutput("no.fls.trt.denosumab.other.female")),
           textOutput("label.no.fls.trt.zoledronate.other.female"),
      tags$h5(uiOutput("no.fls.trt.zoledronate.other.female")),
            textOutput("label.no.fls.trt.teriparatide.other.female"),
      tags$h5(uiOutput("no.fls.trt.teriparatide.other.female")),
           textOutput("label.no.fls.trt.abaloparatide.other.female"),
      tags$h5(uiOutput("no.fls.trt.abaloparatide.other.female")),
           textOutput("label.no.fls.trt.romo.other.female"),
      tags$h5(uiOutput("no.fls.trt.romo.other.female"))
))
 )),

   tabPanel("FLS",
             mainPanel(    
                    tags$h3("Medications after sentinel fracture"),
             tags$hr(),
                       tags$h4(uiOutput("med_fls_text")),
         tags$hr(),
                          a(id = "toggle.meds_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'meds_fls',

      tags$h4("After hip fracture"),
            textOutput("label.fls.trt.hip.male"),
      tags$h5(uiOutput("fls.trt.hip.male")),
           textOutput("label.fls.trt.alendronate.hip.male"),
      tags$h5(uiOutput("fls.trt.alendronate.hip.male")),
      textOutput("label.fls.trt.risedronate.hip.male"),
      tags$h5(uiOutput("fls.trt.risedronate.hip.male")),
       textOutput("label.fls.trt.strontium.hip.male"),
      tags$h5(uiOutput("fls.trt.strontium.hip.male")),
           textOutput("label.fls.trt.ibandronate.hip.male"),
      tags$h5(uiOutput("fls.trt.ibandronate.hip.male")),
           textOutput("label.fls.trt.raloxifene.hip.male"),
      tags$h5(uiOutput("fls.trt.raloxifene.hip.male")),
           textOutput("label.fls.trt.denosumab.hip.male"),
      tags$h5(uiOutput("fls.trt.denosumab.hip.male")),
           textOutput("label.fls.trt.zoledronate.hip.male"),
      tags$h5(uiOutput("fls.trt.zoledronate.hip.male")),
            textOutput("label.fls.trt.teriparatide.hip.male"),
      tags$h5(uiOutput("fls.trt.teriparatide.hip.male")),
           textOutput("label.fls.trt.abaloparatide.hip.male"),
      tags$h5(uiOutput("fls.trt.abaloparatide.hip.male")),
           textOutput("label.fls.trt.romo.hip.male"),
      tags$h5(uiOutput("fls.trt.romo.hip.male")),

                 textOutput("label.fls.trt.hip.female"),
      tags$h5(uiOutput("fls.trt.hip.female")),
           textOutput("label.fls.trt.alendronate.hip.female"),
      tags$h5(uiOutput("fls.trt.alendronate.hip.female")),
      textOutput("label.fls.trt.risedronate.hip.female"),
      tags$h5(uiOutput("fls.trt.risedronate.hip.female")),
       textOutput("label.fls.trt.strontium.hip.female"),
      tags$h5(uiOutput("fls.trt.strontium.hip.female")),
           textOutput("label.fls.trt.ibandronate.hip.female"),
      tags$h5(uiOutput("fls.trt.ibandronate.hip.female")),
           textOutput("label.fls.trt.raloxifene.hip.female"),
      tags$h5(uiOutput("fls.trt.raloxifene.hip.female")),
           textOutput("label.fls.trt.denosumab.hip.female"),
      tags$h5(uiOutput("fls.trt.denosumab.hip.female")),
           textOutput("label.fls.trt.zoledronate.hip.female"),
      tags$h5(uiOutput("fls.trt.zoledronate.hip.female")),
            textOutput("label.fls.trt.teriparatide.hip.female"),
      tags$h5(uiOutput("fls.trt.teriparatide.hip.female")),
           textOutput("label.fls.trt.abaloparatide.hip.female"),
      tags$h5(uiOutput("fls.trt.abaloparatide.hip.female")),
           textOutput("label.fls.trt.romo.hip.female"),
      tags$h5(uiOutput("fls.trt.romo.hip.female")),

            tags$h4("After spine fracture"),
            textOutput("label.fls.trt.spine.male"),
      tags$h5(uiOutput("fls.trt.spine.male")),
           textOutput("label.fls.trt.alendronate.spine.male"),
      tags$h5(uiOutput("fls.trt.alendronate.spine.male")),
      textOutput("label.fls.trt.risedronate.spine.male"),
      tags$h5(uiOutput("fls.trt.risedronate.spine.male")),
       textOutput("label.fls.trt.strontium.spine.male"),
      tags$h5(uiOutput("fls.trt.strontium.spine.male")),
           textOutput("label.fls.trt.ibandronate.spine.male"),
      tags$h5(uiOutput("fls.trt.ibandronate.spine.male")),
           textOutput("label.fls.trt.raloxifene.spine.male"),
      tags$h5(uiOutput("fls.trt.raloxifene.spine.male")),
           textOutput("label.fls.trt.denosumab.spine.male"),
      tags$h5(uiOutput("fls.trt.denosumab.spine.male")),
           textOutput("label.fls.trt.zoledronate.spine.male"),
      tags$h5(uiOutput("fls.trt.zoledronate.spine.male")),
            textOutput("label.fls.trt.teriparatide.spine.male"),
      tags$h5(uiOutput("fls.trt.teriparatide.spine.male")),
           textOutput("label.fls.trt.abaloparatide.spine.male"),
      tags$h5(uiOutput("fls.trt.abaloparatide.spine.male")),
           textOutput("label.fls.trt.romo.spine.male"),
      tags$h5(uiOutput("fls.trt.romo.spine.male")),
      
                  textOutput("label.fls.trt.spine.female"),
      tags$h5(uiOutput("fls.trt.spine.female")),
           textOutput("label.fls.trt.alendronate.spine.female"),
      tags$h5(uiOutput("fls.trt.alendronate.spine.female")),
      textOutput("label.fls.trt.risedronate.spine.female"),
      tags$h5(uiOutput("fls.trt.risedronate.spine.female")),
       textOutput("label.fls.trt.strontium.spine.female"),
      tags$h5(uiOutput("fls.trt.strontium.spine.female")),
           textOutput("label.fls.trt.ibandronate.spine.female"),
      tags$h5(uiOutput("fls.trt.ibandronate.spine.female")),
           textOutput("label.fls.trt.raloxifene.spine.female"),
      tags$h5(uiOutput("fls.trt.raloxifene.spine.female")),
           textOutput("label.fls.trt.denosumab.spine.female"),
      tags$h5(uiOutput("fls.trt.denosumab.spine.female")),
           textOutput("label.fls.trt.zoledronate.spine.female"),
      tags$h5(uiOutput("fls.trt.zoledronate.spine.female")),
            textOutput("label.fls.trt.teriparatide.spine.female"),
      tags$h5(uiOutput("fls.trt.teriparatide.spine.female")),
           textOutput("label.fls.trt.abaloparatide.spine.female"),
      tags$h5(uiOutput("fls.trt.abaloparatide.spine.female")),
           textOutput("label.fls.trt.romo.spine.female"),
      tags$h5(uiOutput("fls.trt.romo.spine.female")),
 
            tags$h4("After other fracture"),
            textOutput("label.fls.trt.other.male"),
      tags$h5(uiOutput("fls.trt.other.male")),
           textOutput("label.fls.trt.alendronate.other.male"),
      tags$h5(uiOutput("fls.trt.alendronate.other.male")),
      textOutput("label.fls.trt.risedronate.other.male"),
      tags$h5(uiOutput("fls.trt.risedronate.other.male")),
       textOutput("label.fls.trt.strontium.other.male"),
      tags$h5(uiOutput("fls.trt.strontium.other.male")),
           textOutput("label.fls.trt.ibandronate.other.male"),
      tags$h5(uiOutput("fls.trt.ibandronate.other.male")),
                 textOutput("label.fls.trt.raloxifene.other.male"),
      tags$h5(uiOutput("fls.trt.raloxifene.other.male")),
           textOutput("label.fls.trt.denosumab.other.male"),
      tags$h5(uiOutput("fls.trt.denosumab.other.male")),
           textOutput("label.fls.trt.zoledronate.other.male"),
      tags$h5(uiOutput("fls.trt.zoledronate.other.male")),
            textOutput("label.fls.trt.teriparatide.other.male"),
      tags$h5(uiOutput("fls.trt.teriparatide.other.male")),
           textOutput("label.fls.trt.abaloparatide.other.male"),
      tags$h5(uiOutput("fls.trt.abaloparatide.other.male")),
           textOutput("label.fls.trt.romo.other.male"),
      tags$h5(uiOutput("fls.trt.romo.other.male")),
 
            textOutput("label.fls.trt.other.female"),
      tags$h5(uiOutput("fls.trt.other.female")),
           textOutput("label.fls.trt.alendronate.other.female"),
      tags$h5(uiOutput("fls.trt.alendronate.other.female")),
      textOutput("label.fls.trt.risedronate.other.female"),
      tags$h5(uiOutput("fls.trt.risedronate.other.female")),
       textOutput("label.fls.trt.strontium.other.female"),
      tags$h5(uiOutput("fls.trt.strontium.other.female")),
           textOutput("label.fls.trt.ibandronate.other.female"),
      tags$h5(uiOutput("fls.trt.ibandronate.other.female")),
           textOutput("label.fls.trt.raloxifene.other.female"),
      tags$h5(uiOutput("fls.trt.raloxifene.other.female")),
           textOutput("label.fls.trt.denosumab.other.female"),
      tags$h5(uiOutput("fls.trt.denosumab.other.female")),
           textOutput("label.fls.trt.zoledronate.other.female"),
      tags$h5(uiOutput("fls.trt.zoledronate.other.female")),
            textOutput("label.fls.trt.teriparatide.other.female"),
      tags$h5(uiOutput("fls.trt.teriparatide.other.female")),
           textOutput("label.fls.trt.abaloparatide.other.female"),
      tags$h5(uiOutput("fls.trt.abaloparatide.other.female")),
           textOutput("label.fls.trt.romo.other.female"),
      tags$h5(uiOutput("fls.trt.romo.other.female"))
))
             ))
       )),
 
  tabPanel("Medications after romosozumab",
         tabsetPanel(type = "pills",
     tabPanel("Current practice",
             mainPanel(     
         tags$h3("Medications after 1 year in romosozumab"),
             tags$hr(),
         
         tags$h4(uiOutput("meds_romo_no_fls_text")),
         tags$hr(),
         
                                   a(id = "toggle.romo.meds_no_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'romo.meds_no_fls',
         
 textOutput("label.romo.to.nothing.no.fls.male"),
      tags$h5(uiOutput("romo.to.nothing.no.fls.male")),
 textOutput("label.romo.to.alendronate.no.fls.male"),
      tags$h5(uiOutput("romo.to.alendronate.no.fls.male")),
  textOutput("label.romo.to.risedronate.no.fls.male"),
      tags$h5(uiOutput("romo.to.risedronate.no.fls.male")),
  textOutput("label.romo.to.strontium.no.fls.male"),
      tags$h5(uiOutput("romo.to.strontium.no.fls.male")),
 textOutput("label.romo.to.ibandronate.no.fls.male"),
      tags$h5(uiOutput("romo.to.ibandronate.no.fls.male")),
  textOutput("label.romo.to.raloxifene.no.fls.male"),
      tags$h5(uiOutput("romo.to.raloxifene.no.fls.male")),
 textOutput("label.romo.to.denosumab.no.fls.male"),
      tags$h5(uiOutput("romo.to.denosumab.no.fls.male")),
 textOutput("label.romo.to.zoledronate.no.fls.male"),
      tags$h5(uiOutput("romo.to.zoledronate.no.fls.male")),


 textOutput("label.romo.to.nothing.no.fls.female"),
      tags$h5(uiOutput("romo.to.nothing.no.fls.female")),
 textOutput("label.romo.to.alendronate.no.fls.female"),
      tags$h5(uiOutput("romo.to.alendronate.no.fls.female")),
  textOutput("label.romo.to.risedronate.no.fls.female"),
      tags$h5(uiOutput("romo.to.risedronate.no.fls.female")),
  textOutput("label.romo.to.strontium.no.fls.female"),
      tags$h5(uiOutput("romo.to.strontium.no.fls.female")),
 textOutput("label.romo.to.ibandronate.no.fls.female"),
      tags$h5(uiOutput("romo.to.ibandronate.no.fls.female")),
 textOutput("label.romo.to.raloxifene.no.fls.female"),
      tags$h5(uiOutput("romo.to.raloxifene.no.fls.female")),
 textOutput("label.romo.to.denosumab.no.fls.female"),
      tags$h5(uiOutput("romo.to.denosumab.no.fls.female")),
 textOutput("label.romo.to.zoledronate.no.fls.female"),
      tags$h5(uiOutput("romo.to.zoledronate.no.fls.female"))
           ))
             )),

  tabPanel("FLS",
             mainPanel(
 
          tags$h3("Medications after 1 year in romosozumab"),
             tags$hr(),
                   tags$h4(uiOutput("meds_romo_fls_text")),
         tags$hr(),
         
          a(id = "toggle.romo.meds_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'romo.meds_fls',
         
  textOutput("label.romo.to.nothing.fls.male"),
      tags$h5(uiOutput("romo.to.nothing.fls.male")),
 textOutput("label.romo.to.alendronate.fls.male"),
      tags$h5(uiOutput("romo.to.alendronate.fls.male")),
  textOutput("label.romo.to.risedronate.fls.male"),
      tags$h5(uiOutput("romo.to.risedronate.fls.male")),
  textOutput("label.romo.to.strontium.fls.male"),
      tags$h5(uiOutput("romo.to.strontium.fls.male")),
 textOutput("label.romo.to.ibandronate.fls.male"),
      tags$h5(uiOutput("romo.to.ibandronate.fls.male")),
  textOutput("label.romo.to.raloxifene.fls.male"),
      tags$h5(uiOutput("romo.to.raloxifene.fls.male")),
 textOutput("label.romo.to.denosumab.fls.male"),
      tags$h5(uiOutput("romo.to.denosumab.fls.male")),
 textOutput("label.romo.to.zoledronate.fls.male"),
      tags$h5(uiOutput("romo.to.zoledronate.fls.male")),


 textOutput("label.romo.to.nothing.fls.female"),
      tags$h5(uiOutput("romo.to.nothing.fls.female")),
 textOutput("label.romo.to.alendronate.fls.female"),
      tags$h5(uiOutput("romo.to.alendronate.fls.female")),
  textOutput("label.romo.to.risedronate.fls.female"),
      tags$h5(uiOutput("romo.to.risedronate.fls.female")),
  textOutput("label.romo.to.strontium.fls.female"),
      tags$h5(uiOutput("romo.to.strontium.fls.female")),
 textOutput("label.romo.to.ibandronate.fls.female"),
      tags$h5(uiOutput("romo.to.ibandronate.fls.female")),
 textOutput("label.romo.to.raloxifene.fls.female"),
      tags$h5(uiOutput("romo.to.raloxifene.fls.female")),
 textOutput("label.romo.to.denosumab.fls.female"),
      tags$h5(uiOutput("romo.to.denosumab.fls.female")),
 textOutput("label.romo.to.zoledronate.fls.female"),
      tags$h5(uiOutput("romo.to.zoledronate.fls.female"))
))
))
       )),

  tabPanel("Medications after abaloparatide",
         tabsetPanel(type = "pills",
     tabPanel("Current practice",
             mainPanel(     
         tags$h3("Medications after 2 years in abaloparatide"),
             tags$hr(),
         
         tags$h4(uiOutput("meds_abaloparatide_no_fls_text")),
         tags$hr(),
         
                                   a(id = "toggle.abaloparatide.meds_no_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'abaloparatide.meds_no_fls',
         
 textOutput("label.abaloparatide.to.nothing.no.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.nothing.no.fls.male")),
 textOutput("label.abaloparatide.to.alendronate.no.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.alendronate.no.fls.male")),
  textOutput("label.abaloparatide.to.risedronate.no.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.risedronate.no.fls.male")),
  textOutput("label.abaloparatide.to.strontium.no.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.strontium.no.fls.male")),
 textOutput("label.abaloparatide.to.ibandronate.no.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.ibandronate.no.fls.male")),
  textOutput("label.abaloparatide.to.raloxifene.no.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.raloxifene.no.fls.male")),
 textOutput("label.abaloparatide.to.denosumab.no.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.denosumab.no.fls.male")),
 textOutput("label.abaloparatide.to.zoledronate.no.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.zoledronate.no.fls.male")),


 textOutput("label.abaloparatide.to.nothing.no.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.nothing.no.fls.female")),
 textOutput("label.abaloparatide.to.alendronate.no.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.alendronate.no.fls.female")),
  textOutput("label.abaloparatide.to.risedronate.no.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.risedronate.no.fls.female")),
  textOutput("label.abaloparatide.to.strontium.no.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.strontium.no.fls.female")),
 textOutput("label.abaloparatide.to.ibandronate.no.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.ibandronate.no.fls.female")),
 textOutput("label.abaloparatide.to.raloxifene.no.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.raloxifene.no.fls.female")),
 textOutput("label.abaloparatide.to.denosumab.no.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.denosumab.no.fls.female")),
 textOutput("label.abaloparatide.to.zoledronate.no.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.zoledronate.no.fls.female"))
           ))
             )),

  tabPanel("FLS",
             mainPanel(
 
          tags$h3("Medications after 2 years in abaloparatide"),
             tags$hr(),
                   tags$h4(uiOutput("meds_abaloparatide_fls_text")),
         tags$hr(),
         
          a(id = "toggle.abaloparatide.meds_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'abaloparatide.meds_fls',
         
  textOutput("label.abaloparatide.to.nothing.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.nothing.fls.male")),
 textOutput("label.abaloparatide.to.alendronate.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.alendronate.fls.male")),
  textOutput("label.abaloparatide.to.risedronate.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.risedronate.fls.male")),
  textOutput("label.abaloparatide.to.strontium.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.strontium.fls.male")),
 textOutput("label.abaloparatide.to.ibandronate.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.ibandronate.fls.male")),
 textOutput("label.abaloparatide.to.raloxifene.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.raloxifene.fls.male")),
 textOutput("label.abaloparatide.to.denosumab.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.denosumab.fls.male")),
 textOutput("label.abaloparatide.to.zoledronate.fls.male"),
      tags$h5(uiOutput("abaloparatide.to.zoledronate.fls.male")),


 textOutput("label.abaloparatide.to.nothing.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.nothing.fls.female")),
 textOutput("label.abaloparatide.to.alendronate.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.alendronate.fls.female")),
  textOutput("label.abaloparatide.to.risedronate.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.risedronate.fls.female")),
  textOutput("label.abaloparatide.to.strontium.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.strontium.fls.female")),
 textOutput("label.abaloparatide.to.ibandronate.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.ibandronate.fls.female")),
 textOutput("label.abaloparatide.to.raloxifene.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.raloxifene.fls.female")),
 textOutput("label.abaloparatide.to.denosumab.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.denosumab.fls.female")),
 textOutput("label.abaloparatide.to.zoledronate.fls.female"),
      tags$h5(uiOutput("abaloparatide.to.zoledronate.fls.female"))
))
))
       )),



  tabPanel("Medications after teriparatide",
         tabsetPanel(type = "pills",
     tabPanel("Current practice",
             mainPanel(     
         tags$h3("Medications after 2 years in teriparatide"),
             tags$hr(),
         
         tags$h4(uiOutput("meds_teriparatide_no_fls_text")),
         tags$hr(),
         
                                   a(id = "toggle.teriparatide.meds_no_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'teriparatide.meds_no_fls',
         
 textOutput("label.teriparatide.to.nothing.no.fls.male"),
      tags$h5(uiOutput("teriparatide.to.nothing.no.fls.male")),
 textOutput("label.teriparatide.to.alendronate.no.fls.male"),
      tags$h5(uiOutput("teriparatide.to.alendronate.no.fls.male")),
  textOutput("label.teriparatide.to.risedronate.no.fls.male"),
      tags$h5(uiOutput("teriparatide.to.risedronate.no.fls.male")),
  textOutput("label.teriparatide.to.strontium.no.fls.male"),
      tags$h5(uiOutput("teriparatide.to.strontium.no.fls.male")),
 textOutput("label.teriparatide.to.ibandronate.no.fls.male"),
      tags$h5(uiOutput("teriparatide.to.ibandronate.no.fls.male")),
 textOutput("label.teriparatide.to.raloxifene.no.fls.male"),
      tags$h5(uiOutput("teriparatide.to.raloxifene.no.fls.male")),
 textOutput("label.teriparatide.to.denosumab.no.fls.male"),
      tags$h5(uiOutput("teriparatide.to.denosumab.no.fls.male")),
 textOutput("label.teriparatide.to.zoledronate.no.fls.male"),
      tags$h5(uiOutput("teriparatide.to.zoledronate.no.fls.male")),


 textOutput("label.teriparatide.to.nothing.no.fls.female"),
      tags$h5(uiOutput("teriparatide.to.nothing.no.fls.female")),
 textOutput("label.teriparatide.to.alendronate.no.fls.female"),
      tags$h5(uiOutput("teriparatide.to.alendronate.no.fls.female")),
  textOutput("label.teriparatide.to.risedronate.no.fls.female"),
      tags$h5(uiOutput("teriparatide.to.risedronate.no.fls.female")),
  textOutput("label.teriparatide.to.strontium.no.fls.female"),
      tags$h5(uiOutput("teriparatide.to.strontium.no.fls.female")),
 textOutput("label.teriparatide.to.ibandronate.no.fls.female"),
      tags$h5(uiOutput("teriparatide.to.ibandronate.no.fls.female")),
 textOutput("label.teriparatide.to.raloxifene.no.fls.female"),
      tags$h5(uiOutput("teriparatide.to.raloxifene.no.fls.female")),
 textOutput("label.teriparatide.to.denosumab.no.fls.female"),
      tags$h5(uiOutput("teriparatide.to.denosumab.no.fls.female")),
 textOutput("label.teriparatide.to.zoledronate.no.fls.female"),
      tags$h5(uiOutput("teriparatide.to.zoledronate.no.fls.female"))
           ))
             )),

  tabPanel("FLS",
             mainPanel(
 
          tags$h3("Medications after 2 years in teriparatide"),
             tags$hr(),
                   tags$h4(uiOutput("meds_teriparatide_fls_text")),
         tags$hr(),
         
          a(id = "toggle.teriparatide.meds_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'teriparatide.meds_fls',
         
  textOutput("label.teriparatide.to.nothing.fls.male"),
      tags$h5(uiOutput("teriparatide.to.nothing.fls.male")),
 textOutput("label.teriparatide.to.alendronate.fls.male"),
      tags$h5(uiOutput("teriparatide.to.alendronate.fls.male")),
  textOutput("label.teriparatide.to.risedronate.fls.male"),
      tags$h5(uiOutput("teriparatide.to.risedronate.fls.male")),
  textOutput("label.teriparatide.to.strontium.fls.male"),
      tags$h5(uiOutput("teriparatide.to.strontium.fls.male")),
 textOutput("label.teriparatide.to.ibandronate.fls.male"),
      tags$h5(uiOutput("teriparatide.to.ibandronate.fls.male")),
 textOutput("label.teriparatide.to.raloxifene.fls.male"),
      tags$h5(uiOutput("teriparatide.to.raloxifene.fls.male")),
 textOutput("label.teriparatide.to.denosumab.fls.male"),
      tags$h5(uiOutput("teriparatide.to.denosumab.fls.male")),
 textOutput("label.teriparatide.to.zoledronate.fls.male"),
      tags$h5(uiOutput("teriparatide.to.zoledronate.fls.male")),


 textOutput("label.teriparatide.to.nothing.fls.female"),
      tags$h5(uiOutput("teriparatide.to.nothing.fls.female")),
 textOutput("label.teriparatide.to.alendronate.fls.female"),
      tags$h5(uiOutput("teriparatide.to.alendronate.fls.female")),
  textOutput("label.teriparatide.to.risedronate.fls.female"),
      tags$h5(uiOutput("teriparatide.to.risedronate.fls.female")),
  textOutput("label.teriparatide.to.strontium.fls.female"),
      tags$h5(uiOutput("teriparatide.to.strontium.fls.female")),
 textOutput("label.teriparatide.to.ibandronate.fls.female"),
      tags$h5(uiOutput("teriparatide.to.ibandronate.fls.female")),
 textOutput("label.teriparatide.to.raloxifene.fls.female"),
      tags$h5(uiOutput("teriparatide.to.raloxifene.fls.female")),
 textOutput("label.teriparatide.to.denosumab.fls.female"),
      tags$h5(uiOutput("teriparatide.to.denosumab.fls.female")),
 textOutput("label.teriparatide.to.zoledronate.fls.female"),
      tags$h5(uiOutput("teriparatide.to.zoledronate.fls.female"))
))
))
       )),




 
  tabPanel("Primary adherence",
                    tabsetPanel(type = "pills",
     tabPanel("Current practice",
             mainPanel(   
           
         tags$h3("Primary adherence"),
             tags$hr(),
                  tags$h4(uiOutput("primary.adh_no_fls_text")),
         tags$hr(),
                   a(id = "toggle.primary.adh_no_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'primary.adh_no_fls',

 
 
textOutput("label.primary.adh_alendronate.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_alendronate.spine.no.fls.male")),
textOutput("label.primary.adh_risedronate.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_risedronate.spine.no.fls.male")),
textOutput("label.primary.adh_strontium.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_strontium.spine.no.fls.male")),
textOutput("label.primary.adh_ibandronate.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_ibandronate.spine.no.fls.male")),
textOutput("label.primary.adh_raloxifene.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_raloxifene.spine.no.fls.male")),
textOutput("label.primary.adh_denosumab.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_denosumab.spine.no.fls.male")),
textOutput("label.primary.adh_zoledronate.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_zoledronate.spine.no.fls.male")),
textOutput("label.primary.adh_teriparatide.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_teriparatide.spine.no.fls.male")),
textOutput("label.primary.adh_abaloparatide.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_abaloparatide.spine.no.fls.male")),
textOutput("label.primary.adh_romo.spine.no.fls.male"),
tags$h5(uiOutput("primary.adh_romo.spine.no.fls.male")),

textOutput("label.primary.adh_alendronate.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_alendronate.hip.no.fls.male")),
textOutput("label.primary.adh_risedronate.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_risedronate.hip.no.fls.male")),
textOutput("label.primary.adh_strontium.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_strontium.hip.no.fls.male")),
textOutput("label.primary.adh_ibandronate.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_ibandronate.hip.no.fls.male")),
textOutput("label.primary.adh_raloxifene.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_raloxifene.hip.no.fls.male")),
textOutput("label.primary.adh_denosumab.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_denosumab.hip.no.fls.male")),
textOutput("label.primary.adh_zoledronate.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_zoledronate.hip.no.fls.male")),
textOutput("label.primary.adh_teriparatide.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_teriparatide.hip.no.fls.male")),
textOutput("label.primary.adh_abaloparatide.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_abaloparatide.hip.no.fls.male")),
textOutput("label.primary.adh_romo.hip.no.fls.male"),
tags$h5(uiOutput("primary.adh_romo.hip.no.fls.male")),

textOutput("label.primary.adh_alendronate.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_alendronate.other.no.fls.male")),
textOutput("label.primary.adh_risedronate.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_risedronate.other.no.fls.male")),
textOutput("label.primary.adh_strontium.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_strontium.other.no.fls.male")),
textOutput("label.primary.adh_ibandronate.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_ibandronate.other.no.fls.male")),
textOutput("label.primary.adh_raloxifene.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_raloxifene.other.no.fls.male")),
textOutput("label.primary.adh_denosumab.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_denosumab.other.no.fls.male")),
textOutput("label.primary.adh_zoledronate.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_zoledronate.other.no.fls.male")),
textOutput("label.primary.adh_teriparatide.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_teriparatide.other.no.fls.male")),
textOutput("label.primary.adh_abaloparatide.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_abaloparatide.other.no.fls.male")),
textOutput("label.primary.adh_romo.other.no.fls.male"),
tags$h5(uiOutput("primary.adh_romo.other.no.fls.male")),



textOutput("label.primary.adh_alendronate.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_alendronate.spine.no.fls.female")),
textOutput("label.primary.adh_risedronate.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_risedronate.spine.no.fls.female")),
textOutput("label.primary.adh_strontium.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_strontium.spine.no.fls.female")),
textOutput("label.primary.adh_ibandronate.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_ibandronate.spine.no.fls.female")),
textOutput("label.primary.adh_raloxifene.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_raloxifene.spine.no.fls.female")),
textOutput("label.primary.adh_denosumab.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_denosumab.spine.no.fls.female")),
textOutput("label.primary.adh_zoledronate.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_zoledronate.spine.no.fls.female")),
textOutput("label.primary.adh_teriparatide.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_teriparatide.spine.no.fls.female")),
textOutput("label.primary.adh_abaloparatide.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_abaloparatide.spine.no.fls.female")),
textOutput("label.primary.adh_romo.spine.no.fls.female"),
tags$h5(uiOutput("primary.adh_romo.spine.no.fls.female")),

textOutput("label.primary.adh_alendronate.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_alendronate.hip.no.fls.female")),
textOutput("label.primary.adh_risedronate.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_risedronate.hip.no.fls.female")),
textOutput("label.primary.adh_strontium.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_strontium.hip.no.fls.female")),
textOutput("label.primary.adh_ibandronate.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_ibandronate.hip.no.fls.female")),
textOutput("label.primary.adh_raloxifene.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_raloxifene.hip.no.fls.female")),
textOutput("label.primary.adh_denosumab.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_denosumab.hip.no.fls.female")),
textOutput("label.primary.adh_zoledronate.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_zoledronate.hip.no.fls.female")),
textOutput("label.primary.adh_teriparatide.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_teriparatide.hip.no.fls.female")),
textOutput("label.primary.adh_abaloparatide.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_abaloparatide.hip.no.fls.female")),
textOutput("label.primary.adh_romo.hip.no.fls.female"),
tags$h5(uiOutput("primary.adh_romo.hip.no.fls.female")),

textOutput("label.primary.adh_alendronate.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_alendronate.other.no.fls.female")),
textOutput("label.primary.adh_risedronate.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_risedronate.other.no.fls.female")),
textOutput("label.primary.adh_strontium.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_strontium.other.no.fls.female")),
textOutput("label.primary.adh_ibandronate.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_ibandronate.other.no.fls.female")),
textOutput("label.primary.adh_raloxifene.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_raloxifene.other.no.fls.female")),
textOutput("label.primary.adh_denosumab.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_denosumab.other.no.fls.female")),
textOutput("label.primary.adh_zoledronate.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_zoledronate.other.no.fls.female")),
textOutput("label.primary.adh_teriparatide.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_teriparatide.other.no.fls.female")),
textOutput("label.primary.adh_abaloparatide.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_abaloparatide.other.no.fls.female")),
textOutput("label.primary.adh_romo.other.no.fls.female"),
tags$h5(uiOutput("primary.adh_romo.other.no.fls.female"))

))
)),
     tabPanel("FLS",
             mainPanel( 
                        tags$h3("Primary adherence"),
             tags$hr(),
                               tags$h4(uiOutput("primary.adh_fls_text")),
         tags$hr(),
                            a(id = "toggle.primary.adh_fls",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'primary.adh_fls',

textOutput("label.primary.adh_alendronate.spine.fls.male"),
tags$h5(uiOutput("primary.adh_alendronate.spine.fls.male")),
textOutput("label.primary.adh_risedronate.spine.fls.male"),
tags$h5(uiOutput("primary.adh_risedronate.spine.fls.male")),
textOutput("label.primary.adh_strontium.spine.fls.male"),
tags$h5(uiOutput("primary.adh_strontium.spine.fls.male")),
textOutput("label.primary.adh_ibandronate.spine.fls.male"),
tags$h5(uiOutput("primary.adh_ibandronate.spine.fls.male")),
textOutput("label.primary.adh_raloxifene.spine.fls.male"),
tags$h5(uiOutput("primary.adh_raloxifene.spine.fls.male")),
textOutput("label.primary.adh_denosumab.spine.fls.male"),
tags$h5(uiOutput("primary.adh_denosumab.spine.fls.male")),
textOutput("label.primary.adh_zoledronate.spine.fls.male"),
tags$h5(uiOutput("primary.adh_zoledronate.spine.fls.male")),
textOutput("label.primary.adh_teriparatide.spine.fls.male"),
tags$h5(uiOutput("primary.adh_teriparatide.spine.fls.male")),
textOutput("label.primary.adh_abaloparatide.spine.fls.male"),
tags$h5(uiOutput("primary.adh_abaloparatide.spine.fls.male")),
textOutput("label.primary.adh_romo.spine.fls.male"),
tags$h5(uiOutput("primary.adh_romo.spine.fls.male")),

textOutput("label.primary.adh_alendronate.hip.fls.male"),
tags$h5(uiOutput("primary.adh_alendronate.hip.fls.male")),
textOutput("label.primary.adh_risedronate.hip.fls.male"),
tags$h5(uiOutput("primary.adh_risedronate.hip.fls.male")),
textOutput("label.primary.adh_strontium.hip.fls.male"),
tags$h5(uiOutput("primary.adh_strontium.hip.fls.male")),
textOutput("label.primary.adh_ibandronate.hip.fls.male"),
tags$h5(uiOutput("primary.adh_ibandronate.hip.fls.male")),
textOutput("label.primary.adh_raloxifene.hip.fls.male"),
tags$h5(uiOutput("primary.adh_raloxifene.hip.fls.male")),
textOutput("label.primary.adh_denosumab.hip.fls.male"),
tags$h5(uiOutput("primary.adh_denosumab.hip.fls.male")),
textOutput("label.primary.adh_zoledronate.hip.fls.male"),
tags$h5(uiOutput("primary.adh_zoledronate.hip.fls.male")),
textOutput("label.primary.adh_teriparatide.hip.fls.male"),
tags$h5(uiOutput("primary.adh_teriparatide.hip.fls.male")),
textOutput("label.primary.adh_abaloparatide.hip.fls.male"),
tags$h5(uiOutput("primary.adh_abaloparatide.hip.fls.male")),
textOutput("label.primary.adh_romo.hip.fls.male"),
tags$h5(uiOutput("primary.adh_romo.hip.fls.male")),

textOutput("label.primary.adh_alendronate.other.fls.male"),
tags$h5(uiOutput("primary.adh_alendronate.other.fls.male")),
textOutput("label.primary.adh_risedronate.other.fls.male"),
tags$h5(uiOutput("primary.adh_risedronate.other.fls.male")),
textOutput("label.primary.adh_strontium.other.fls.male"),
tags$h5(uiOutput("primary.adh_strontium.other.fls.male")),
textOutput("label.primary.adh_ibandronate.other.fls.male"),
tags$h5(uiOutput("primary.adh_ibandronate.other.fls.male")),
textOutput("label.primary.adh_raloxifene.other.fls.male"),
tags$h5(uiOutput("primary.adh_raloxifene.other.fls.male")),
textOutput("label.primary.adh_denosumab.other.fls.male"),
tags$h5(uiOutput("primary.adh_denosumab.other.fls.male")),
textOutput("label.primary.adh_zoledronate.other.fls.male"),
tags$h5(uiOutput("primary.adh_zoledronate.other.fls.male")),
textOutput("label.primary.adh_teriparatide.other.fls.male"),
tags$h5(uiOutput("primary.adh_teriparatide.other.fls.male")),
textOutput("label.primary.adh_abaloparatide.other.fls.male"),
tags$h5(uiOutput("primary.adh_abaloparatide.other.fls.male")),
textOutput("label.primary.adh_romo.other.fls.male"),
tags$h5(uiOutput("primary.adh_romo.other.fls.male")),



textOutput("label.primary.adh_alendronate.spine.fls.female"),
tags$h5(uiOutput("primary.adh_alendronate.spine.fls.female")),
textOutput("label.primary.adh_risedronate.spine.fls.female"),
tags$h5(uiOutput("primary.adh_risedronate.spine.fls.female")),
textOutput("label.primary.adh_strontium.spine.fls.female"),
tags$h5(uiOutput("primary.adh_strontium.spine.fls.female")),
textOutput("label.primary.adh_ibandronate.spine.fls.female"),
tags$h5(uiOutput("primary.adh_ibandronate.spine.fls.female")),
textOutput("label.primary.adh_raloxifene.spine.fls.female"),
tags$h5(uiOutput("primary.adh_raloxifene.spine.fls.female")),
textOutput("label.primary.adh_denosumab.spine.fls.female"),
tags$h5(uiOutput("primary.adh_denosumab.spine.fls.female")),
textOutput("label.primary.adh_zoledronate.spine.fls.female"),
tags$h5(uiOutput("primary.adh_zoledronate.spine.fls.female")),
textOutput("label.primary.adh_teriparatide.spine.fls.female"),
tags$h5(uiOutput("primary.adh_teriparatide.spine.fls.female")),
textOutput("label.primary.adh_abaloparatide.spine.fls.female"),
tags$h5(uiOutput("primary.adh_abaloparatide.spine.fls.female")),
textOutput("label.primary.adh_romo.spine.fls.female"),
tags$h5(uiOutput("primary.adh_romo.spine.fls.female")),

textOutput("label.primary.adh_alendronate.hip.fls.female"),
tags$h5(uiOutput("primary.adh_alendronate.hip.fls.female")),
textOutput("label.primary.adh_risedronate.hip.fls.female"),
tags$h5(uiOutput("primary.adh_risedronate.hip.fls.female")),
textOutput("label.primary.adh_strontium.hip.fls.female"),
tags$h5(uiOutput("primary.adh_strontium.hip.fls.female")),
textOutput("label.primary.adh_ibandronate.hip.fls.female"),
tags$h5(uiOutput("primary.adh_ibandronate.hip.fls.female")),
textOutput("label.primary.adh_raloxifene.hip.fls.female"),
tags$h5(uiOutput("primary.adh_raloxifene.hip.fls.female")),
textOutput("label.primary.adh_denosumab.hip.fls.female"),
tags$h5(uiOutput("primary.adh_denosumab.hip.fls.female")),
textOutput("label.primary.adh_zoledronate.hip.fls.female"),
tags$h5(uiOutput("primary.adh_zoledronate.hip.fls.female")),
textOutput("label.primary.adh_teriparatide.hip.fls.female"),
tags$h5(uiOutput("primary.adh_teriparatide.hip.fls.female")),
textOutput("label.primary.adh_abaloparatide.hip.fls.female"),
tags$h5(uiOutput("primary.adh_abaloparatide.hip.fls.female")),
textOutput("label.primary.adh_romo.hip.fls.female"),
tags$h5(uiOutput("primary.adh_romo.hip.fls.female")),

textOutput("label.primary.adh_alendronate.other.fls.female"),
tags$h5(uiOutput("primary.adh_alendronate.other.fls.female")),
textOutput("label.primary.adh_risedronate.other.fls.female"),
tags$h5(uiOutput("primary.adh_risedronate.other.fls.female")),
textOutput("label.primary.adh_strontium.other.fls.female"),
tags$h5(uiOutput("primary.adh_strontium.other.fls.female")),
textOutput("label.primary.adh_ibandronate.other.fls.female"),
tags$h5(uiOutput("primary.adh_ibandronate.other.fls.female")),
textOutput("label.primary.adh_raloxifene.other.fls.female"),
tags$h5(uiOutput("primary.adh_raloxifene.other.fls.female")),
textOutput("label.primary.adh_denosumab.other.fls.female"),
tags$h5(uiOutput("primary.adh_denosumab.other.fls.female")),
textOutput("label.primary.adh_zoledronate.other.fls.female"),
tags$h5(uiOutput("primary.adh_zoledronate.other.fls.female")),
textOutput("label.primary.adh_teriparatide.other.fls.female"),
tags$h5(uiOutput("primary.adh_teriparatide.other.fls.female")),
textOutput("label.primary.adh_abaloparatide.other.fls.female"),
tags$h5(uiOutput("primary.adh_abaloparatide.other.fls.female")),
textOutput("label.primary.adh_romo.other.fls.female"),
tags$h5(uiOutput("primary.adh_romo.other.fls.female"))
         ))
             ))
)),




 tabPanel("Monitoring",
         tabsetPanel(type = "pills",
     tabPanel("Current practice",
             mainPanel(     
         tags$h3("Monitoring"),
         tags$hr(),
         tags$h4(uiOutput("monitoring.no.fls_text")),
         tags$hr(),

          a(id = "toggle.no.fls.monitored",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'no.fls.monitored',
               textOutput("label.no.fls.monitored.spine.4m.male"),
      tags$h5(uiOutput("no.fls.monitored.spine.4m.male")),
          textOutput("label.no.fls.monitored.hip.4m.male"),
      tags$h5(uiOutput("no.fls.monitored.hip.4m.male")),
          textOutput("label.no.fls.monitored.other.4m.male"),
      tags$h5(uiOutput("no.fls.monitored.other.4m.male")),
     
          textOutput("label.no.fls.monitored.spine.12m.male"),
      tags$h5(uiOutput("no.fls.monitored.spine.12m.male")),
          textOutput("label.no.fls.monitored.hip.12m.male"),
      tags$h5(uiOutput("no.fls.monitored.hip.12m.male")),
          textOutput("label.no.fls.monitored.other.12m.male"),
      tags$h5(uiOutput("no.fls.monitored.other.12m.male")),
     
          textOutput("label.no.fls.monitored.spine.4m.female"),
      tags$h5(uiOutput("no.fls.monitored.spine.4m.female")),
          textOutput("label.no.fls.monitored.hip.4m.female"),
      tags$h5(uiOutput("no.fls.monitored.hip.4m.female")),
          textOutput("label.no.fls.monitored.other.4m.female"),
      tags$h5(uiOutput("no.fls.monitored.other.4m.female")),
     
          textOutput("label.no.fls.monitored.spine.12m.female"),
      tags$h5(uiOutput("no.fls.monitored.spine.12m.female")),
          textOutput("label.no.fls.monitored.hip.12m.female"),
      tags$h5(uiOutput("no.fls.monitored.hip.12m.female")),
          textOutput("label.no.fls.monitored.other.12m.female"),
      tags$h5(uiOutput("no.fls.monitored.other.12m.female"))

     
     

           ))
             ))
             ,

  tabPanel("FLS",
             mainPanel(
 
          tags$h3("Monitoring"),
             tags$hr(),
                   tags$h4(uiOutput("monitoring.fls_text")),
         tags$hr(),
                    a(id = "toggle.fls.monitored",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'fls.monitored',
               textOutput("label.fls.monitored.spine.4m.male"),
      tags$h5(uiOutput("fls.monitored.spine.4m.male")),
          textOutput("label.fls.monitored.hip.4m.male"),
      tags$h5(uiOutput("fls.monitored.hip.4m.male")),
          textOutput("label.fls.monitored.other.4m.male"),
      tags$h5(uiOutput("fls.monitored.other.4m.male")),
     
          textOutput("label.fls.monitored.spine.12m.male"),
      tags$h5(uiOutput("fls.monitored.spine.12m.male")),
          textOutput("label.fls.monitored.hip.12m.male"),
      tags$h5(uiOutput("fls.monitored.hip.12m.male")),
          textOutput("label.fls.monitored.other.12m.male"),
      tags$h5(uiOutput("fls.monitored.other.12m.male")),
     
          textOutput("label.fls.monitored.spine.4m.female"),
      tags$h5(uiOutput("fls.monitored.spine.4m.female")),
          textOutput("label.fls.monitored.hip.4m.female"),
      tags$h5(uiOutput("fls.monitored.hip.4m.female")),
          textOutput("label.fls.monitored.other.4m.female"),
      tags$h5(uiOutput("fls.monitored.other.4m.female")),
     
          textOutput("label.fls.monitored.spine.12m.female"),
      tags$h5(uiOutput("fls.monitored.spine.12m.female")),
          textOutput("label.fls.monitored.hip.12m.female"),
      tags$h5(uiOutput("fls.monitored.hip.12m.female")),
          textOutput("label.fls.monitored.other.12m.female"),
      tags$h5(uiOutput("fls.monitored.other.12m.female"))

 
))
))
       )),



 tabPanel("Adherence first year",
                    tabsetPanel(type = "pills",
     tabPanel("Monitored",
             mainPanel(  
         tags$h3("Adherence first year"),
              tags$hr(),
         tags$h4(uiOutput("adh.first.year.monitored_text")),
         tags$hr(),
         a(id = "toggle.monitored.adh.first.year",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'monitored.adh.first.year',
textOutput("label.monitored.4m_adh_alendronate.male"),
tags$h5(uiOutput("monitored.4m_adh_alendronate.male")),      
textOutput("label.monitored.4m_adh_risedronate.male"),
tags$h5(uiOutput("monitored.4m_adh_risedronate.male")),      
textOutput("label.monitored.4m_adh_strontium.male"),
tags$h5(uiOutput("monitored.4m_adh_strontium.male")),      
textOutput("label.monitored.4m_adh_ibandronate.male"),
tags$h5(uiOutput("monitored.4m_adh_ibandronate.male")),        
textOutput("label.monitored.4m_adh_raloxifene.male"),
tags$h5(uiOutput("monitored.4m_adh_raloxifene.male")),     
textOutput("label.monitored.4m_adh_denosumab.male"),
tags$h5(uiOutput("monitored.4m_adh_denosumab.male")),      
textOutput("label.monitored.4m_adh_zoledronate.male"),
tags$h5(uiOutput("monitored.4m_adh_zoledronate.male")),      
textOutput("label.monitored.4m_adh_teriparatide.male"),
tags$h5(uiOutput("monitored.4m_adh_teriparatide.male")),      
textOutput("label.monitored.4m_adh_abaloparatide.male"),
tags$h5(uiOutput("monitored.4m_adh_abaloparatide.male")),      
textOutput("label.monitored.4m_adh_romo.male"),
tags$h5(uiOutput("monitored.4m_adh_romo.male")),      
         
textOutput("label.monitored.12m_adh_alendronate.male"),
tags$h5(uiOutput("monitored.12m_adh_alendronate.male")),      
textOutput("label.monitored.12m_adh_risedronate.male"),
tags$h5(uiOutput("monitored.12m_adh_risedronate.male")),      
textOutput("label.monitored.12m_adh_strontium.male"),
tags$h5(uiOutput("monitored.12m_adh_strontium.male")),      
textOutput("label.monitored.12m_adh_ibandronate.male"),
tags$h5(uiOutput("monitored.12m_adh_ibandronate.male")),      
textOutput("label.monitored.12m_adh_raloxifene.male"),
tags$h5(uiOutput("monitored.12m_adh_raloxifene.male")),     
textOutput("label.monitored.12m_adh_denosumab.male"),
tags$h5(uiOutput("monitored.12m_adh_denosumab.male")),      
textOutput("label.monitored.12m_adh_zoledronate.male"),
tags$h5(uiOutput("monitored.12m_adh_zoledronate.male")),      
textOutput("label.monitored.12m_adh_teriparatide.male"),
tags$h5(uiOutput("monitored.12m_adh_teriparatide.male")),      
textOutput("label.monitored.12m_adh_abaloparatide.male"),
tags$h5(uiOutput("monitored.12m_adh_abaloparatide.male")),  


textOutput("label.monitored.4m_adh_alendronate.female"),
tags$h5(uiOutput("monitored.4m_adh_alendronate.female")),      
textOutput("label.monitored.4m_adh_risedronate.female"),
tags$h5(uiOutput("monitored.4m_adh_risedronate.female")),      
textOutput("label.monitored.4m_adh_strontium.female"),
tags$h5(uiOutput("monitored.4m_adh_strontium.female")),      
textOutput("label.monitored.4m_adh_ibandronate.female"),
tags$h5(uiOutput("monitored.4m_adh_ibandronate.female")),      
textOutput("label.monitored.4m_adh_raloxifene.female"),
tags$h5(uiOutput("monitored.4m_adh_raloxifene.female")),      
textOutput("label.monitored.4m_adh_denosumab.female"),
tags$h5(uiOutput("monitored.4m_adh_denosumab.female")),      
textOutput("label.monitored.4m_adh_zoledronate.female"),
tags$h5(uiOutput("monitored.4m_adh_zoledronate.female")),      
textOutput("label.monitored.4m_adh_teriparatide.female"),
tags$h5(uiOutput("monitored.4m_adh_teriparatide.female")),      
textOutput("label.monitored.4m_adh_abaloparatide.female"),
tags$h5(uiOutput("monitored.4m_adh_abaloparatide.female")),      
textOutput("label.monitored.4m_adh_romo.female"),
tags$h5(uiOutput("monitored.4m_adh_romo.female")),      
         
textOutput("label.monitored.12m_adh_alendronate.female"),
tags$h5(uiOutput("monitored.12m_adh_alendronate.female")),      
textOutput("label.monitored.12m_adh_risedronate.female"),
tags$h5(uiOutput("monitored.12m_adh_risedronate.female")),      
textOutput("label.monitored.12m_adh_strontium.female"),
tags$h5(uiOutput("monitored.12m_adh_strontium.female")),      
textOutput("label.monitored.12m_adh_ibandronate.female"),
tags$h5(uiOutput("monitored.12m_adh_ibandronate.female")),      
textOutput("label.monitored.12m_adh_raloxifene.female"),
tags$h5(uiOutput("monitored.12m_adh_raloxifene.female")),     
textOutput("label.monitored.12m_adh_denosumab.female"),
tags$h5(uiOutput("monitored.12m_adh_denosumab.female")),      
textOutput("label.monitored.12m_adh_zoledronate.female"),
tags$h5(uiOutput("monitored.12m_adh_zoledronate.female")),      
textOutput("label.monitored.12m_adh_teriparatide.female"),
tags$h5(uiOutput("monitored.12m_adh_teriparatide.female")),      
textOutput("label.monitored.12m_adh_abaloparatide.female"),
tags$h5(uiOutput("monitored.12m_adh_abaloparatide.female"))

))
             )),
     tabPanel("Not monitored",
             mainPanel(   
           
         tags$h3("Adherence first year"),
         
                       tags$hr(),
          tags$h4(uiOutput("adh.first.year.not.monitored_text")),
         tags$hr(),

         
     a(id = "toggle.not.monitored.adh.first.year",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'not.monitored.adh.first.year',
         
textOutput("label.not.monitored.4m_adh_alendronate.male"),
tags$h5(uiOutput("not.monitored.4m_adh_alendronate.male")),      
textOutput("label.not.monitored.4m_adh_risedronate.male"),
tags$h5(uiOutput("not.monitored.4m_adh_risedronate.male")),      
textOutput("label.not.monitored.4m_adh_strontium.male"),
tags$h5(uiOutput("not.monitored.4m_adh_strontium.male")),      
textOutput("label.not.monitored.4m_adh_ibandronate.male"),
tags$h5(uiOutput("not.monitored.4m_adh_ibandronate.male")),     
textOutput("label.not.monitored.4m_adh_raloxifene.male"),
tags$h5(uiOutput("not.monitored.4m_adh_raloxifene.male")),      
textOutput("label.not.monitored.4m_adh_denosumab.male"),
tags$h5(uiOutput("not.monitored.4m_adh_denosumab.male")),      
textOutput("label.not.monitored.4m_adh_zoledronate.male"),
tags$h5(uiOutput("not.monitored.4m_adh_zoledronate.male")),      
textOutput("label.not.monitored.4m_adh_teriparatide.male"),
tags$h5(uiOutput("not.monitored.4m_adh_teriparatide.male")),      
textOutput("label.not.monitored.4m_adh_abaloparatide.male"),
tags$h5(uiOutput("not.monitored.4m_adh_abaloparatide.male")),      
textOutput("label.not.monitored.4m_adh_romo.male"),
tags$h5(uiOutput("not.monitored.4m_adh_romo.male")),      
         
textOutput("label.not.monitored.12m_adh_alendronate.male"),
tags$h5(uiOutput("not.monitored.12m_adh_alendronate.male")),      
textOutput("label.not.monitored.12m_adh_risedronate.male"),
tags$h5(uiOutput("not.monitored.12m_adh_risedronate.male")),      
textOutput("label.not.monitored.12m_adh_strontium.male"),
tags$h5(uiOutput("not.monitored.12m_adh_strontium.male")),      
textOutput("label.not.monitored.12m_adh_ibandronate.male"),
tags$h5(uiOutput("not.monitored.12m_adh_ibandronate.male")),     
textOutput("label.not.monitored.12m_adh_raloxifene.male"),
tags$h5(uiOutput("not.monitored.12m_adh_raloxifene.male")),     
textOutput("label.not.monitored.12m_adh_denosumab.male"),
tags$h5(uiOutput("not.monitored.12m_adh_denosumab.male")),      
textOutput("label.not.monitored.12m_adh_zoledronate.male"),
tags$h5(uiOutput("not.monitored.12m_adh_zoledronate.male")),      
textOutput("label.not.monitored.12m_adh_teriparatide.male"),
tags$h5(uiOutput("not.monitored.12m_adh_teriparatide.male")),      
textOutput("label.not.monitored.12m_adh_abaloparatide.male"),
tags$h5(uiOutput("not.monitored.12m_adh_abaloparatide.male")),  


textOutput("label.not.monitored.4m_adh_alendronate.female"),
tags$h5(uiOutput("not.monitored.4m_adh_alendronate.female")),      
textOutput("label.not.monitored.4m_adh_risedronate.female"),
tags$h5(uiOutput("not.monitored.4m_adh_risedronate.female")),      
textOutput("label.not.monitored.4m_adh_strontium.female"),
tags$h5(uiOutput("not.monitored.4m_adh_strontium.female")),      
textOutput("label.not.monitored.4m_adh_ibandronate.female"),
tags$h5(uiOutput("not.monitored.4m_adh_ibandronate.female")),       
textOutput("label.not.monitored.4m_adh_raloxifene.female"),
tags$h5(uiOutput("not.monitored.4m_adh_raloxifene.female")),     
textOutput("label.not.monitored.4m_adh_denosumab.female"),
tags$h5(uiOutput("not.monitored.4m_adh_denosumab.female")),      
textOutput("label.not.monitored.4m_adh_zoledronate.female"),
tags$h5(uiOutput("not.monitored.4m_adh_zoledronate.female")),      
textOutput("label.not.monitored.4m_adh_teriparatide.female"),
tags$h5(uiOutput("not.monitored.4m_adh_teriparatide.female")),      
textOutput("label.not.monitored.4m_adh_abaloparatide.female"),
tags$h5(uiOutput("not.monitored.4m_adh_abaloparatide.female")),      
textOutput("label.not.monitored.4m_adh_romo.female"),
tags$h5(uiOutput("not.monitored.4m_adh_romo.female")),      
         
textOutput("label.not.monitored.12m_adh_alendronate.female"),
tags$h5(uiOutput("not.monitored.12m_adh_alendronate.female")),      
textOutput("label.not.monitored.12m_adh_risedronate.female"),
tags$h5(uiOutput("not.monitored.12m_adh_risedronate.female")),      
textOutput("label.not.monitored.12m_adh_strontium.female"),
tags$h5(uiOutput("not.monitored.12m_adh_strontium.female")),      
textOutput("label.not.monitored.12m_adh_ibandronate.female"),
tags$h5(uiOutput("not.monitored.12m_adh_ibandronate.female")),       
textOutput("label.not.monitored.12m_adh_raloxifene.female"),
tags$h5(uiOutput("not.monitored.12m_adh_raloxifene.female")),      
textOutput("label.not.monitored.12m_adh_denosumab.female"),
tags$h5(uiOutput("not.monitored.12m_adh_denosumab.female")),      
textOutput("label.not.monitored.12m_adh_zoledronate.female"),
tags$h5(uiOutput("not.monitored.12m_adh_zoledronate.female")),      
textOutput("label.not.monitored.12m_adh_teriparatide.female"),
tags$h5(uiOutput("not.monitored.12m_adh_teriparatide.female")),      
textOutput("label.not.monitored.12m_adh_abaloparatide.female"),
tags$h5(uiOutput("not.monitored.12m_adh_abaloparatide.female"))         
             ))
))
     
     )),


 tabPanel("Adherence after second year",
                    tabsetPanel(type = "pills",
     tabPanel("Current practice",
             mainPanel(  
                        tags$h3("Adherence after second year"),

               
                       tags$hr(),
          tags$h4(uiOutput("adh.second.year.on.no.fls_text")),
         tags$hr(),

      a(id = "toggle.no.fls.adh.second.year.on",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'no.fls.adh.second.year.on',
         
textOutput("label.adh_annual_decline_alendronate.no.fls.male"),
tags$h5(uiOutput("adh_annual_decline_alendronate.no.fls.male")),   
textOutput("label.adh_annual_decline_risedronate.no.fls.male"),
tags$h5(uiOutput("adh_annual_decline_risedronate.no.fls.male")),         
textOutput("label.adh_annual_decline_strontium.no.fls.male"),
tags$h5(uiOutput("adh_annual_decline_strontium.no.fls.male")),   
textOutput("label.adh_annual_decline_ibandronate.no.fls.male"),
tags$h5(uiOutput("adh_annual_decline_ibandronate.no.fls.male")),    
textOutput("label.adh_annual_decline_raloxifene.no.fls.male"),
tags$h5(uiOutput("adh_annual_decline_raloxifene.no.fls.male")),
textOutput("label.adh_annual_decline_denosumab.no.fls.male"),
tags$h5(uiOutput("adh_annual_decline_denosumab.no.fls.male")),   
textOutput("label.adh_annual_decline_zoledronate.no.fls.male"),
tags$h5(uiOutput("adh_annual_decline_zoledronate.no.fls.male")),   
        
 textOutput("label.adh_annual_decline_alendronate.no.fls.female"),
tags$h5(uiOutput("adh_annual_decline_alendronate.no.fls.female")),   
textOutput("label.adh_annual_decline_risedronate.no.fls.female"),
tags$h5(uiOutput("adh_annual_decline_risedronate.no.fls.female")),         
textOutput("label.adh_annual_decline_strontium.no.fls.female"),
tags$h5(uiOutput("adh_annual_decline_strontium.no.fls.female")),   
textOutput("label.adh_annual_decline_ibandronate.no.fls.female"),
tags$h5(uiOutput("adh_annual_decline_ibandronate.no.fls.female")),  
textOutput("label.adh_annual_decline_raloxifene.no.fls.female"),
tags$h5(uiOutput("adh_annual_decline_raloxifene.no.fls.female")),   
textOutput("label.adh_annual_decline_denosumab.no.fls.female"),
tags$h5(uiOutput("adh_annual_decline_denosumab.no.fls.female")),   
textOutput("label.adh_annual_decline_zoledronate.no.fls.female"),
tags$h5(uiOutput("adh_annual_decline_zoledronate.no.fls.female"))
           ))
         )),
     
     tabPanel("FLS",
             mainPanel(  
                tags$h3("Adherence after second year"),

                       tags$hr(),
          tags$h4(uiOutput("adh.second.year.on.fls_text")),
         tags$hr(),
      a(id = "toggle.fls.adh.second.year.on",
        tags$h6("View/ edit")),
         shinyjs::hidden(
           tags$div(id = 'fls.adh.second.year.on',

textOutput("label.adh_annual_decline_alendronate.fls.male"),
tags$h5(uiOutput("adh_annual_decline_alendronate.fls.male")),   
textOutput("label.adh_annual_decline_risedronate.fls.male"),
tags$h5(uiOutput("adh_annual_decline_risedronate.fls.male")),         
textOutput("label.adh_annual_decline_strontium.fls.male"),
tags$h5(uiOutput("adh_annual_decline_strontium.fls.male")),   
textOutput("label.adh_annual_decline_ibandronate.fls.male"),
tags$h5(uiOutput("adh_annual_decline_ibandronate.fls.male")),  
textOutput("label.adh_annual_decline_raloxifene.fls.male"),
tags$h5(uiOutput("adh_annual_decline_raloxifene.fls.male")),   
textOutput("label.adh_annual_decline_denosumab.fls.male"),
tags$h5(uiOutput("adh_annual_decline_denosumab.fls.male")),   
textOutput("label.adh_annual_decline_zoledronate.fls.male"),
tags$h5(uiOutput("adh_annual_decline_zoledronate.fls.male")),   
        
 textOutput("label.adh_annual_decline_alendronate.fls.female"),
tags$h5(uiOutput("adh_annual_decline_alendronate.fls.female")),   
textOutput("label.adh_annual_decline_risedronate.fls.female"),
tags$h5(uiOutput("adh_annual_decline_risedronate.fls.female")),         
textOutput("label.adh_annual_decline_strontium.fls.female"),
tags$h5(uiOutput("adh_annual_decline_strontium.fls.female")),   
textOutput("label.adh_annual_decline_ibandronate.fls.female"),
tags$h5(uiOutput("adh_annual_decline_ibandronate.fls.female")),    
textOutput("label.adh_annual_decline_raloxifene.fls.female"),
tags$h5(uiOutput("adh_annual_decline_raloxifene.fls.female")),  
textOutput("label.adh_annual_decline_denosumab.fls.female"),
tags$h5(uiOutput("adh_annual_decline_denosumab.fls.female")),   
textOutput("label.adh_annual_decline_zoledronate.fls.female"),
tags$h5(uiOutput("adh_annual_decline_zoledronate.fls.female"))         
         
           ))
         ))
     
     ))

)),
## Costing -----
tabPanel("Costing",
    tabsetPanel(type = "tabs",
          tabPanel("Treatment following fracture",
mainPanel(

  tags$h3("Treatment following fracture"),
  tags$h4("After a hip fracture"),
   textOutput("label.prop.admitted.surgery.hip"),
      tags$h5(uiOutput("prop.admitted.surgery.hip")),
   textOutput("label.prop.admitted.no.surgery.hip"),
      tags$h5(uiOutput("prop.admitted.no.surgery.hip")),
    textOutput("label.prop.not.admitted.clinic.hip"),
      tags$h5(uiOutput("prop.not.admitted.clinic.hip")),
 tags$hr(),

  tags$h4("After a spine fracture"),
   textOutput("label.prop.hospital.spine"),
      tags$h5(uiOutput("prop.hospital.spine")),
   textOutput("label.prop.hospital.community.spine"),
      tags$h5(uiOutput("prop.hospital.community.spine")),

   tags$h4("And specifically for those spine fractures
           leading to a hospital visit"),
   textOutput("label.prop.hospital.spine.kyphoplasty"),
      tags$h5(uiOutput("prop.hospital.spine.kyphoplasty")),
  textOutput("label.prop.hospital.spine.vertebroplasty"),
      tags$h5(uiOutput("prop.hospital.spine.vertebroplasty")),
  textOutput("label.prop.hospital.spine.no.intervention"),
      tags$h5(uiOutput("prop.hospital.spine.no.intervention")),
  textOutput("label.prop.not.admitted.spine.kyphoplasty"),
      tags$h5(uiOutput("prop.not.admitted.spine.kyphoplasty")),
  textOutput("label.prop.not.admitted.spine.vertebroplasty"),
      tags$h5(uiOutput("prop.not.admitted.spine.vertebroplasty")),
  textOutput("label.prop.not.admitted.spine.no.intervention"),
      tags$h5(uiOutput("prop.not.admitted.spine.no.intervention")),
 tags$hr(),
 tags$h4("After a other fracture"),
textOutput("label.prop.admitted.surgery.other"),
     tags$h5(uiOutput("prop.admitted.surgery.other")),
textOutput("label.prop.admitted.no.surgery.other"),
     tags$h5(uiOutput("prop.admitted.no.surgery.other")),
textOutput("label.prop.not.admitted.clinic.other"),
     tags$h5(uiOutput("prop.not.admitted.clinic.other")),
 tags$hr()
)),

  tabPanel("Hospital legnth of stay",
           mainPanel(
   tags$h3("Hospital legnth of stay (for those admitted)"),
   
textOutput("label.hospital.los.hip.surg"),
     tags$h5(uiOutput("hospital.los.hip.surg")),
textOutput("label.hospital.los.hip.no.surg"),
     tags$h5(uiOutput("hospital.los.hip.no.surg")),
textOutput("label.hospital.los.spine.kyphoplasty"),
     tags$h5(uiOutput("hospital.los.spine.kyphoplasty")),
textOutput("label.hospital.los.spine.vertebroplasty"),
     tags$h5(uiOutput("hospital.los.spine.vertebroplasty")),
textOutput("label.hospital.los.spine.no.surg"),
     tags$h5(uiOutput("hospital.los.spine.no.surg")),
textOutput("label.hospital.los.other.surg"),
     tags$h5(uiOutput("hospital.los.other.surg")),
textOutput("label.hospital.los.other.no.surg"),
     tags$h5(uiOutput("hospital.los.other.no.surg"))

)),

  tabPanel("Hospital costs",
mainPanel(
  tags$h3("Hospital costs"),
  tags$h4("Generic"),
  textOutput("label.cost.a_e.visit"),
     tags$h5(uiOutput("cost.a_e.visit")),
  textOutput("label.cost.hosp.per.day"),
     tags$h5(uiOutput("cost.hosp.per.day")),
  textOutput("label.cost.hosp.clinic.visit"),
     tags$h5(uiOutput("cost.hosp.clinic.visit")),

 tags$hr(),
 tags$h4("Site specific"),
 
  textOutput("label.cost.hip.surg"),
     tags$h5(uiOutput("cost.hip.surg")),
  textOutput("label.cost.spine.kyphoplasty"),
     tags$h5(uiOutput("cost.spine.kyphoplasty")),
  textOutput("label.cost.spine.vertebroplasty"),
     tags$h5(uiOutput("cost.spine.vertebroplasty")),
  textOutput("label.cost.other.surg"),
     tags$h5(uiOutput("cost.other.surg"))

 )),

  tabPanel("Community care",
mainPanel(
  tags$h3("Community care after spine fractures"),
  tags$h4("Consultations"),
  
  textOutput("label.visits.comm.consults.spine"),
     tags$h5(uiOutput("visits.comm.consults.spine")),
  tags$h4("Cost"),
  textOutput("label.cost.spine.community.care"),
     tags$h5(uiOutput("cost.spine.community.care"))

)),

  tabPanel("Fracture prevention support: staff time",

  tabsetPanel(type = "pills",
     tabPanel("Current practice",

       mainPanel(
 tags$h3("Fracture prevention support: staff time under current practice"),
  tags$h4("Average time spent by stage"),
tags$h4("Following hip fracture"),
tags$h4("Administrator"),

  textOutput("label.no.fls.administrator.identification.mins.hip"),
     tags$h5(uiOutput("no.fls.administrator.identification.mins.hip")),
  textOutput("label.no.fls.administrator.assessment.mins.hip"),
     tags$h5(uiOutput("no.fls.administrator.assessment.mins.hip")),
  textOutput("label.no.fls.administrator.recommendation.mins.hip"),
     tags$h5(uiOutput("no.fls.administrator.recommendation.mins.hip")),
  textOutput("label.no.fls.administrator.monitoring.mins.hip"),
     tags$h5(uiOutput("no.fls.administrator.monitoring.mins.hip")),
tags$hr(),

tags$h5("Nurse"),
 textOutput("label.no.fls.nurse.identification.mins.hip"),
     tags$h5(uiOutput("no.fls.nurse.identification.mins.hip")),
 textOutput("label.no.fls.nurse.assessment.mins.hip"),
     tags$h5(uiOutput("no.fls.nurse.assessment.mins.hip")),
 textOutput("label.no.fls.nurse.recommendation.mins.hip"),
     tags$h5(uiOutput("no.fls.nurse.recommendation.mins.hip")),
 textOutput("label.no.fls.nurse.monitoring.mins.hip"),
     tags$h5(uiOutput("no.fls.nurse.monitoring.mins.hip")),
tags$hr(),

tags$h5("Doctor"),
 textOutput("label.no.fls.doctor.identification.mins.hip"),
     tags$h5(uiOutput("no.fls.doctor.identification.mins.hip")),
 textOutput("label.no.fls.doctor.assessment.mins.hip"),
     tags$h5(uiOutput("no.fls.doctor.assessment.mins.hip")),
 textOutput("label.no.fls.doctor.recommendation.mins.hip"),
     tags$h5(uiOutput("no.fls.doctor.recommendation.mins.hip")),
 textOutput("label.no.fls.doctor.monitoring.mins.hip"),
     tags$h5(uiOutput("no.fls.doctor.monitoring.mins.hip")),
tags$hr(),

tags$h5("Radiographer"),
 textOutput("label.no.fls.radiographer.identification.mins.hip"),
     tags$h5(uiOutput("no.fls.radiographer.identification.mins.hip")),
 textOutput("label.no.fls.radiographer.assessment.mins.hip"),
     tags$h5(uiOutput("no.fls.radiographer.assessment.mins.hip")),
 textOutput("label.no.fls.radiographer.recommendation.mins.hip"),
     tags$h5(uiOutput("no.fls.radiographer.recommendation.mins.hip")),
 textOutput("label.no.fls.radiographer.monitoring.mins.hip"),
     tags$h5(uiOutput("no.fls.radiographer.monitoring.mins.hip")),
tags$hr(),

tags$h5("Allied health professional"),
 textOutput("label.no.fls.allied_health.identification.mins.hip"),
     tags$h5(uiOutput("no.fls.allied_health.identification.mins.hip")),
 textOutput("label.no.fls.allied_health.assessment.mins.hip"),
     tags$h5(uiOutput("no.fls.allied_health.assessment.mins.hip")),
 textOutput("label.no.fls.allied_health.recommendation.mins.hip"),
     tags$h5(uiOutput("no.fls.allied_health.recommendation.mins.hip")),
 textOutput("label.no.fls.allied_health.monitoring.mins.hip"),
     tags$h5(uiOutput("no.fls.allied_health.monitoring.mins.hip")),
tags$hr(),

tags$h5("Other"),
 textOutput("label.no.fls.other.identification.mins.hip"),
     tags$h5(uiOutput("no.fls.other.identification.mins.hip")),
 textOutput("label.no.fls.other.assessment.mins.hip"),
     tags$h5(uiOutput("no.fls.other.assessment.mins.hip")),
 textOutput("label.no.fls.other.recommendation.mins.hip"),
     tags$h5(uiOutput("no.fls.other.recommendation.mins.hip")),
 textOutput("label.no.fls.other.monitoring.mins.hip"),
     tags$h5(uiOutput("no.fls.other.monitoring.mins.hip")),
tags$hr(),


tags$h4("Following spine fracture"),
tags$h4("Administrator"),

  textOutput("label.no.fls.administrator.identification.mins.spine"),
     tags$h5(uiOutput("no.fls.administrator.identification.mins.spine")),
  textOutput("label.no.fls.administrator.assessment.mins.spine"),
     tags$h5(uiOutput("no.fls.administrator.assessment.mins.spine")),
  textOutput("label.no.fls.administrator.recommendation.mins.spine"),
     tags$h5(uiOutput("no.fls.administrator.recommendation.mins.spine")),
  textOutput("label.no.fls.administrator.monitoring.mins.spine"),
     tags$h5(uiOutput("no.fls.administrator.monitoring.mins.spine")),
tags$hr(),

tags$h5("Nurse"),
 textOutput("label.no.fls.nurse.identification.mins.spine"),
     tags$h5(uiOutput("no.fls.nurse.identification.mins.spine")),
 textOutput("label.no.fls.nurse.assessment.mins.spine"),
     tags$h5(uiOutput("no.fls.nurse.assessment.mins.spine")),
 textOutput("label.no.fls.nurse.recommendation.mins.spine"),
     tags$h5(uiOutput("no.fls.nurse.recommendation.mins.spine")),
 textOutput("label.no.fls.nurse.monitoring.mins.spine"),
     tags$h5(uiOutput("no.fls.nurse.monitoring.mins.spine")),
tags$hr(),

tags$h5("Doctor"),
 textOutput("label.no.fls.doctor.identification.mins.spine"),
     tags$h5(uiOutput("no.fls.doctor.identification.mins.spine")),
 textOutput("label.no.fls.doctor.assessment.mins.spine"),
     tags$h5(uiOutput("no.fls.doctor.assessment.mins.spine")),
 textOutput("label.no.fls.doctor.recommendation.mins.spine"),
     tags$h5(uiOutput("no.fls.doctor.recommendation.mins.spine")),
 textOutput("label.no.fls.doctor.monitoring.mins.spine"),
     tags$h5(uiOutput("no.fls.doctor.monitoring.mins.spine")),
tags$hr(),

tags$h5("Radiographer"),
 textOutput("label.no.fls.radiographer.identification.mins.spine"),
     tags$h5(uiOutput("no.fls.radiographer.identification.mins.spine")),
 textOutput("label.no.fls.radiographer.assessment.mins.spine"),
     tags$h5(uiOutput("no.fls.radiographer.assessment.mins.spine")),
 textOutput("label.no.fls.radiographer.recommendation.mins.spine"),
     tags$h5(uiOutput("no.fls.radiographer.recommendation.mins.spine")),
 textOutput("label.no.fls.radiographer.monitoring.mins.spine"),
     tags$h5(uiOutput("no.fls.radiographer.monitoring.mins.spine")),
tags$hr(),

tags$h5("Allied health professional"),
 textOutput("label.no.fls.allied_health.identification.mins.spine"),
     tags$h5(uiOutput("no.fls.allied_health.identification.mins.spine")),
 textOutput("label.no.fls.allied_health.assessment.mins.spine"),
     tags$h5(uiOutput("no.fls.allied_health.assessment.mins.spine")),
 textOutput("label.no.fls.allied_health.recommendation.mins.spine"),
     tags$h5(uiOutput("no.fls.allied_health.recommendation.mins.spine")),
 textOutput("label.no.fls.allied_health.monitoring.mins.spine"),
     tags$h5(uiOutput("no.fls.allied_health.monitoring.mins.spine")),
tags$hr(),
tags$h5("Other"),
 textOutput("label.no.fls.other.identification.mins.spine"),
     tags$h5(uiOutput("no.fls.other.identification.mins.spine")),
 textOutput("label.no.fls.other.assessment.mins.spine"),
     tags$h5(uiOutput("no.fls.other.assessment.mins.spine")),
 textOutput("label.no.fls.other.recommendation.mins.spine"),
     tags$h5(uiOutput("no.fls.other.recommendation.mins.spine")),
 textOutput("label.no.fls.other.monitoring.mins.spine"),
     tags$h5(uiOutput("no.fls.other.monitoring.mins.spine")),
tags$hr(),

tags$h4("Following other fracture"),
tags$h4("Administrator"),

  textOutput("label.no.fls.administrator.identification.mins.other"),
     tags$h5(uiOutput("no.fls.administrator.identification.mins.other")),
  textOutput("label.no.fls.administrator.assessment.mins.other"),
     tags$h5(uiOutput("no.fls.administrator.assessment.mins.other")),
  textOutput("label.no.fls.administrator.recommendation.mins.other"),
     tags$h5(uiOutput("no.fls.administrator.recommendation.mins.other")),
  textOutput("label.no.fls.administrator.monitoring.mins.other"),
     tags$h5(uiOutput("no.fls.administrator.monitoring.mins.other")),
tags$hr(),

tags$h5("Nurse"),
 textOutput("label.no.fls.nurse.identification.mins.other"),
     tags$h5(uiOutput("no.fls.nurse.identification.mins.other")),
 textOutput("label.no.fls.nurse.assessment.mins.other"),
     tags$h5(uiOutput("no.fls.nurse.assessment.mins.other")),
 textOutput("label.no.fls.nurse.recommendation.mins.other"),
     tags$h5(uiOutput("no.fls.nurse.recommendation.mins.other")),
 textOutput("label.no.fls.nurse.monitoring.mins.other"),
     tags$h5(uiOutput("no.fls.nurse.monitoring.mins.other")),
tags$hr(),

tags$h5("Doctor"),
 textOutput("label.no.fls.doctor.identification.mins.other"),
     tags$h5(uiOutput("no.fls.doctor.identification.mins.other")),
 textOutput("label.no.fls.doctor.assessment.mins.other"),
     tags$h5(uiOutput("no.fls.doctor.assessment.mins.other")),
 textOutput("label.no.fls.doctor.recommendation.mins.other"),
     tags$h5(uiOutput("no.fls.doctor.recommendation.mins.other")),
 textOutput("label.no.fls.doctor.monitoring.mins.other"),
     tags$h5(uiOutput("no.fls.doctor.monitoring.mins.other")),
tags$hr(),

tags$h5("Radiographer"),
 textOutput("label.no.fls.radiographer.identification.mins.other"),
     tags$h5(uiOutput("no.fls.radiographer.identification.mins.other")),
 textOutput("label.no.fls.radiographer.assessment.mins.other"),
     tags$h5(uiOutput("no.fls.radiographer.assessment.mins.other")),
 textOutput("label.no.fls.radiographer.recommendation.mins.other"),
     tags$h5(uiOutput("no.fls.radiographer.recommendation.mins.other")),
 textOutput("label.no.fls.radiographer.monitoring.mins.other"),
     tags$h5(uiOutput("no.fls.radiographer.monitoring.mins.other")),
tags$hr(),

tags$h5("Allied health professional"),
 textOutput("label.no.fls.allied_health.identification.mins.other"),
     tags$h5(uiOutput("no.fls.allied_health.identification.mins.other")),
 textOutput("label.no.fls.allied_health.assessment.mins.other"),
     tags$h5(uiOutput("no.fls.allied_health.assessment.mins.other")),
 textOutput("label.no.fls.allied_health.recommendation.mins.other"),
     tags$h5(uiOutput("no.fls.allied_health.recommendation.mins.other")),
 textOutput("label.no.fls.allied_health.monitoring.mins.other"),
     tags$h5(uiOutput("no.fls.allied_health.monitoring.mins.other")),
tags$hr(),

tags$h5("Other"),
 textOutput("label.no.fls.other.identification.mins.other"),
     tags$h5(uiOutput("no.fls.other.identification.mins.other")),
 textOutput("label.no.fls.other.assessment.mins.other"),
     tags$h5(uiOutput("no.fls.other.assessment.mins.other")),
 textOutput("label.no.fls.other.recommendation.mins.other"),
     tags$h5(uiOutput("no.fls.other.recommendation.mins.other")),
 textOutput("label.no.fls.other.monitoring.mins.other"),
     tags$h5(uiOutput("no.fls.other.monitoring.mins.other")),
tags$hr()
)),





     tabPanel("FLS",

       mainPanel(
 tags$h3("Fracture prevention support: staff time under FLS"),
  tags$h4("Average time spent by stage"),
tags$h4("Following hip fracture"),
tags$h4("Administrator"),

  textOutput("label.fls.administrator.identification.mins.hip"),
     tags$h5(uiOutput("fls.administrator.identification.mins.hip")),
  textOutput("label.fls.administrator.assessment.mins.hip"),
     tags$h5(uiOutput("fls.administrator.assessment.mins.hip")),
  textOutput("label.fls.administrator.recommendation.mins.hip"),
     tags$h5(uiOutput("fls.administrator.recommendation.mins.hip")),
  textOutput("label.fls.administrator.monitoring.mins.hip"),
     tags$h5(uiOutput("fls.administrator.monitoring.mins.hip")),
tags$hr(),


tags$h5("FLS coordinator"),
  textOutput("label.fls.fls_coordinator.identification.mins.hip"),
     tags$h5(uiOutput("fls.fls_coordinator.identification.mins.hip")),
  textOutput("label.fls.fls_coordinator.assessment.mins.hip"),
     tags$h5(uiOutput("fls.fls_coordinator.assessment.mins.hip")),
  textOutput("label.fls.fls_coordinator.recommendation.mins.hip"),
     tags$h5(uiOutput("fls.fls_coordinator.recommendation.mins.hip")),
  textOutput("label.fls.fls_coordinator.monitoring.mins.hip"),
     tags$h5(uiOutput("fls.fls_coordinator.monitoring.mins.hip")),
tags$hr(),

tags$h5("Nurse"),
 textOutput("label.fls.nurse.identification.mins.hip"),
     tags$h5(uiOutput("fls.nurse.identification.mins.hip")),
 textOutput("label.fls.nurse.assessment.mins.hip"),
     tags$h5(uiOutput("fls.nurse.assessment.mins.hip")),
 textOutput("label.fls.nurse.recommendation.mins.hip"),
     tags$h5(uiOutput("fls.nurse.recommendation.mins.hip")),
 textOutput("label.fls.nurse.monitoring.mins.hip"),
     tags$h5(uiOutput("fls.nurse.monitoring.mins.hip")),
tags$hr(),

tags$h5("Doctor"),
 textOutput("label.fls.doctor.identification.mins.hip"),
     tags$h5(uiOutput("fls.doctor.identification.mins.hip")),
 textOutput("label.fls.doctor.assessment.mins.hip"),
     tags$h5(uiOutput("fls.doctor.assessment.mins.hip")),
 textOutput("label.fls.doctor.recommendation.mins.hip"),
     tags$h5(uiOutput("fls.doctor.recommendation.mins.hip")),
 textOutput("label.fls.doctor.monitoring.mins.hip"),
     tags$h5(uiOutput("fls.doctor.monitoring.mins.hip")),
tags$hr(),

tags$h5("Radiographer"),
 textOutput("label.fls.radiographer.identification.mins.hip"),
     tags$h5(uiOutput("fls.radiographer.identification.mins.hip")),
 textOutput("label.fls.radiographer.assessment.mins.hip"),
     tags$h5(uiOutput("fls.radiographer.assessment.mins.hip")),
 textOutput("label.fls.radiographer.recommendation.mins.hip"),
     tags$h5(uiOutput("fls.radiographer.recommendation.mins.hip")),
 textOutput("label.fls.radiographer.monitoring.mins.hip"),
     tags$h5(uiOutput("fls.radiographer.monitoring.mins.hip")),
tags$hr(),

tags$h5("Allied health professional"),
 textOutput("label.fls.allied_health.identification.mins.hip"),
     tags$h5(uiOutput("fls.allied_health.identification.mins.hip")),
 textOutput("label.fls.allied_health.assessment.mins.hip"),
     tags$h5(uiOutput("fls.allied_health.assessment.mins.hip")),
 textOutput("label.fls.allied_health.recommendation.mins.hip"),
     tags$h5(uiOutput("fls.allied_health.recommendation.mins.hip")),
 textOutput("label.fls.allied_health.monitoring.mins.hip"),
     tags$h5(uiOutput("fls.allied_health.monitoring.mins.hip")),
tags$hr(),

tags$h5("Other"),
 textOutput("label.fls.other.identification.mins.hip"),
     tags$h5(uiOutput("fls.other.identification.mins.hip")),
 textOutput("label.fls.other.assessment.mins.hip"),
     tags$h5(uiOutput("fls.other.assessment.mins.hip")),
 textOutput("label.fls.other.recommendation.mins.hip"),
     tags$h5(uiOutput("fls.other.recommendation.mins.hip")),
 textOutput("label.fls.other.monitoring.mins.hip"),
     tags$h5(uiOutput("fls.other.monitoring.mins.hip")),
tags$hr(),



tags$h4("Following spine fracture"),
tags$h4("Administrator"),

  textOutput("label.fls.administrator.identification.mins.spine"),
     tags$h5(uiOutput("fls.administrator.identification.mins.spine")),
  textOutput("label.fls.administrator.assessment.mins.spine"),
     tags$h5(uiOutput("fls.administrator.assessment.mins.spine")),
  textOutput("label.fls.administrator.recommendation.mins.spine"),
     tags$h5(uiOutput("fls.administrator.recommendation.mins.spine")),
  textOutput("label.fls.administrator.monitoring.mins.spine"),
     tags$h5(uiOutput("fls.administrator.monitoring.mins.spine")),
tags$hr(),


tags$h5("FLS coordinator"),
  textOutput("label.fls.fls_coordinator.identification.mins.spine"),
     tags$h5(uiOutput("fls.fls_coordinator.identification.mins.spine")),
  textOutput("label.fls.fls_coordinator.assessment.mins.spine"),
     tags$h5(uiOutput("fls.fls_coordinator.assessment.mins.spine")),
  textOutput("label.fls.fls_coordinator.recommendation.mins.spine"),
     tags$h5(uiOutput("fls.fls_coordinator.recommendation.mins.spine")),
  textOutput("label.fls.fls_coordinator.monitoring.mins.spine"),
     tags$h5(uiOutput("fls.fls_coordinator.monitoring.mins.spine")),
tags$hr(),

tags$h5("Nurse"),
 textOutput("label.fls.nurse.identification.mins.spine"),
     tags$h5(uiOutput("fls.nurse.identification.mins.spine")),
 textOutput("label.fls.nurse.assessment.mins.spine"),
     tags$h5(uiOutput("fls.nurse.assessment.mins.spine")),
 textOutput("label.fls.nurse.recommendation.mins.spine"),
     tags$h5(uiOutput("fls.nurse.recommendation.mins.spine")),
 textOutput("label.fls.nurse.monitoring.mins.spine"),
     tags$h5(uiOutput("fls.nurse.monitoring.mins.spine")),
tags$hr(),

tags$h5("Doctor"),
 textOutput("label.fls.doctor.identification.mins.spine"),
     tags$h5(uiOutput("fls.doctor.identification.mins.spine")),
 textOutput("label.fls.doctor.assessment.mins.spine"),
     tags$h5(uiOutput("fls.doctor.assessment.mins.spine")),
 textOutput("label.fls.doctor.recommendation.mins.spine"),
     tags$h5(uiOutput("fls.doctor.recommendation.mins.spine")),
 textOutput("label.fls.doctor.monitoring.mins.spine"),
     tags$h5(uiOutput("fls.doctor.monitoring.mins.spine")),
tags$hr(),

tags$h5("Radiographer"),
 textOutput("label.fls.radiographer.identification.mins.spine"),
     tags$h5(uiOutput("fls.radiographer.identification.mins.spine")),
 textOutput("label.fls.radiographer.assessment.mins.spine"),
     tags$h5(uiOutput("fls.radiographer.assessment.mins.spine")),
 textOutput("label.fls.radiographer.recommendation.mins.spine"),
     tags$h5(uiOutput("fls.radiographer.recommendation.mins.spine")),
 textOutput("label.fls.radiographer.monitoring.mins.spine"),
     tags$h5(uiOutput("fls.radiographer.monitoring.mins.spine")),
tags$hr(),

tags$h5("Allied health professional"),
 textOutput("label.fls.allied_health.identification.mins.spine"),
     tags$h5(uiOutput("fls.allied_health.identification.mins.spine")),
 textOutput("label.fls.allied_health.assessment.mins.spine"),
     tags$h5(uiOutput("fls.allied_health.assessment.mins.spine")),
 textOutput("label.fls.allied_health.recommendation.mins.spine"),
     tags$h5(uiOutput("fls.allied_health.recommendation.mins.spine")),
 textOutput("label.fls.allied_health.monitoring.mins.spine"),
     tags$h5(uiOutput("fls.allied_health.monitoring.mins.spine")),
tags$hr(),

tags$h5("Other"),
 textOutput("label.fls.other.identification.mins.spine"),
     tags$h5(uiOutput("fls.other.identification.mins.spine")),
 textOutput("label.fls.other.assessment.mins.spine"),
     tags$h5(uiOutput("fls.other.assessment.mins.spine")),
 textOutput("label.fls.other.recommendation.mins.spine"),
     tags$h5(uiOutput("fls.other.recommendation.mins.spine")),
 textOutput("label.fls.other.monitoring.mins.spine"),
     tags$h5(uiOutput("fls.other.monitoring.mins.spine")),
tags$hr(),

tags$h4("Following other fracture"),
tags$h4("Administrator"),

  textOutput("label.fls.administrator.identification.mins.other"),
     tags$h5(uiOutput("fls.administrator.identification.mins.other")),
  textOutput("label.fls.administrator.assessment.mins.other"),
     tags$h5(uiOutput("fls.administrator.assessment.mins.other")),
  textOutput("label.fls.administrator.recommendation.mins.other"),
     tags$h5(uiOutput("fls.administrator.recommendation.mins.other")),
  textOutput("label.fls.administrator.monitoring.mins.other"),
     tags$h5(uiOutput("fls.administrator.monitoring.mins.other")),
tags$hr(),


tags$h5("FLS coordinator"),
  textOutput("label.fls.fls_coordinator.identification.mins.other"),
     tags$h5(uiOutput("fls.fls_coordinator.identification.mins.other")),
  textOutput("label.fls.fls_coordinator.assessment.mins.other"),
     tags$h5(uiOutput("fls.fls_coordinator.assessment.mins.other")),
  textOutput("label.fls.fls_coordinator.recommendation.mins.other"),
     tags$h5(uiOutput("fls.fls_coordinator.recommendation.mins.other")),
  textOutput("label.fls.fls_coordinator.monitoring.mins.other"),
     tags$h5(uiOutput("fls.fls_coordinator.monitoring.mins.other")),
tags$hr(),

tags$h5("Nurse"),
 textOutput("label.fls.nurse.identification.mins.other"),
     tags$h5(uiOutput("fls.nurse.identification.mins.other")),
 textOutput("label.fls.nurse.assessment.mins.other"),
     tags$h5(uiOutput("fls.nurse.assessment.mins.other")),
 textOutput("label.fls.nurse.recommendation.mins.other"),
     tags$h5(uiOutput("fls.nurse.recommendation.mins.other")),
 textOutput("label.fls.nurse.monitoring.mins.other"),
     tags$h5(uiOutput("fls.nurse.monitoring.mins.other")),
tags$hr(),

tags$h5("Doctor"),
 textOutput("label.fls.doctor.identification.mins.other"),
     tags$h5(uiOutput("fls.doctor.identification.mins.other")),
 textOutput("label.fls.doctor.assessment.mins.other"),
     tags$h5(uiOutput("fls.doctor.assessment.mins.other")),
 textOutput("label.fls.doctor.recommendation.mins.other"),
     tags$h5(uiOutput("fls.doctor.recommendation.mins.other")),
 textOutput("label.fls.doctor.monitoring.mins.other"),
     tags$h5(uiOutput("fls.doctor.monitoring.mins.other")),
tags$hr(),

tags$h5("Radiographer"),
 textOutput("label.fls.radiographer.identification.mins.other"),
     tags$h5(uiOutput("fls.radiographer.identification.mins.other")),
 textOutput("label.fls.radiographer.assessment.mins.other"),
     tags$h5(uiOutput("fls.radiographer.assessment.mins.other")),
 textOutput("label.fls.radiographer.recommendation.mins.other"),
     tags$h5(uiOutput("fls.radiographer.recommendation.mins.other")),
 textOutput("label.fls.radiographer.monitoring.mins.other"),
     tags$h5(uiOutput("fls.radiographer.monitoring.mins.other")),
tags$hr(),

tags$h5("Allied health professional"),
 textOutput("label.fls.allied_health.identification.mins.other"),
     tags$h5(uiOutput("fls.allied_health.identification.mins.other")),
 textOutput("label.fls.allied_health.assessment.mins.other"),
     tags$h5(uiOutput("fls.allied_health.assessment.mins.other")),
 textOutput("label.fls.allied_health.recommendation.mins.other"),
     tags$h5(uiOutput("fls.allied_health.recommendation.mins.other")),
 textOutput("label.fls.allied_health.monitoring.mins.other"),
     tags$h5(uiOutput("fls.allied_health.monitoring.mins.other")),
tags$hr(),

tags$h5("Other"),
 textOutput("label.fls.other.identification.mins.other"),
     tags$h5(uiOutput("fls.other.identification.mins.other")),
 textOutput("label.fls.other.assessment.mins.other"),
     tags$h5(uiOutput("fls.other.assessment.mins.other")),
 textOutput("label.fls.other.recommendation.mins.other"),
     tags$h5(uiOutput("fls.other.recommendation.mins.other")),
 textOutput("label.fls.other.monitoring.mins.other"),
     tags$h5(uiOutput("fls.other.monitoring.mins.other")),
tags$hr()

))

)),

tabPanel("Fracture prevention support: staff costs",

mainPanel(
  tags$h3("Fracture prevention support: staff costs"),
      textOutput("label.hourly.cost.administrator"),
      tags$h5(uiOutput("hourly.cost.administrator")),
      textOutput("label.hourly.cost.nurse"),
      tags$h5(uiOutput("hourly.cost.nurse")),
      textOutput("label.hourly.cost.doctor"),
      tags$h5(uiOutput("hourly.cost.doctor")),
      textOutput("label.hourly.cost.radiographer"),
      tags$h5(uiOutput("hourly.cost.radiographer")),
      textOutput("label.hourly.cost.allied_health"),
      tags$h5(uiOutput("hourly.cost.allied_health")),
      textOutput("label.hourly.cost.other"),
      tags$h5(uiOutput("hourly.cost.other")),
      tags$hr(),
      textOutput("label.hourly.cost.fls_coordinator"),
      tags$h5(uiOutput("hourly.cost.fls_coordinator"))
       )),



  tabPanel("Fracture prevention: laboratory test use",

        tabsetPanel(type = "pills",
     tabPanel("Current practice",
            mainPanel(
              
tags$h3("Fracture prevention: laboratory test use under current practice"),
tags$h4("Proportion receiving a laboratory test"),

 textOutput("label.prop.lab.test.no.fls.hip"),
tags$h5(uiOutput("prop.lab.test.no.fls.hip")),   
 textOutput("label.prop.lab.test.no.fls.spine"),
tags$h5(uiOutput("prop.lab.test.no.fls.spine")),
 textOutput("label.prop.lab.test.no.fls.other"),
tags$h5(uiOutput("prop.lab.test.no.fls.other"))
            )  ),

     tabPanel("FLS",
             mainPanel(
       tags$h3("Fracture prevention: laboratory test use under FLS"),
tags$h4("Proportion receiving a laboratory test"),

 textOutput("label.prop.lab.test.fls.hip"),
tags$h5(uiOutput("prop.lab.test.fls.hip")),
 textOutput("label.prop.lab.test.fls.spine"),
tags$h5(uiOutput("prop.lab.test.fls.spine")),
 textOutput("label.prop.lab.test.fls.other"),
tags$h5(uiOutput("prop.lab.test.fls.other"))
             )  )

  )),

  tabPanel("Fracture prevention: laboratory test costs",
    mainPanel(
       tags$h3("Fracture prevention: laboratory test costs"),
 textOutput("label.cost.lab.test.hip"),
tags$h5(uiOutput("cost.lab.test.hip")),
 textOutput("label.cost.lab.test.spine"),
tags$h5(uiOutput("cost.lab.test.spine")),
 textOutput("label.cost.lab.test.other"),
tags$h5(uiOutput("cost.lab.test.other"))
  )),

  tabPanel("Fracture prevention: DXA use",
       tabsetPanel(type = "pills",
     tabPanel("Current practice",
            mainPanel(

 tags$h3("Fracture prevention: DXA use under FLS"),
tags$h4("Proportion receiving a DXA"),

 textOutput("label.prop.dxa.no.fls.hip"),
tags$h5(uiOutput("prop.dxa.no.fls.hip")),
 textOutput("label.prop.dxa.no.fls.spine"),
tags$h5(uiOutput("prop.dxa.no.fls.spine")),
 textOutput("label.prop.dxa.no.fls.other"),
tags$h5(uiOutput("prop.dxa.no.fls.other"))
            )  ),

     tabPanel("FLS",
             mainPanel(
       tags$h3("Fracture prevention: DXA use under FLS"),
tags$h4("Proportion receiving a DXA"),

 textOutput("label.prop.dxa.fls.hip"),
tags$h5(uiOutput("prop.dxa.fls.hip")),
 textOutput("label.prop.dxa.fls.spine"),
tags$h5(uiOutput("prop.dxa.fls.spine")),
 textOutput("label.prop.dxa.fls.other"),
tags$h5(uiOutput("prop.dxa.fls.other"))

             ))

  )),
  tabPanel("Fracture prevention: DXA cost",
    mainPanel(
             tags$h3("Fracture prevention: DXA cost"),

 textOutput("label.cost.dxa"),
tags$h5(uiOutput("cost.dxa"))

  )),

  tabPanel("Fracture prevention: medication costs",
    mainPanel(
  tags$h3("Fracture prevention: medication costs"),
  tags$h4("After hip fracture"),
   textOutput("label.Alendronate.yearly.cost.hip"),
tags$h5(uiOutput("Alendronate.yearly.cost.hip")),
   textOutput("label.Risedronate.yearly.cost.hip"),
tags$h5(uiOutput("Risedronate.yearly.cost.hip")),
     textOutput("label.Ibandronate.yearly.cost.hip"),
tags$h5(uiOutput("Ibandronate.yearly.cost.hip")),
     textOutput("label.Raloxifene.yearly.cost.hip"),
tags$h5(uiOutput("Raloxifene.yearly.cost.hip")),
     textOutput("label.Strontium.yearly.cost.hip"),
tags$h5(uiOutput("Strontium.yearly.cost.hip")),
     textOutput("label.Denosumab.yearly.cost.hip"),
tags$h5(uiOutput("Denosumab.yearly.cost.hip")),
   textOutput("label.Zoledronate.yearly.cost.hip"),
tags$h5(uiOutput("Zoledronate.yearly.cost.hip")),
   textOutput("label.Teriparatide.yearly.cost.hip"),
tags$h5(uiOutput("Teriparatide.yearly.cost.hip")),
   textOutput("label.Abaloparatide.yearly.cost.hip"),
tags$h5(uiOutput("Abaloparatide.yearly.cost.hip")),
   textOutput("label.Romo.yearly.cost.hip"),
tags$h5(uiOutput("Romo.yearly.cost.hip")),

  tags$h4("After spine fracture"),
   textOutput("label.Alendronate.yearly.cost.spine"),
tags$h5(uiOutput("Alendronate.yearly.cost.spine")),
   textOutput("label.Risedronate.yearly.cost.spine"),
tags$h5(uiOutput("Risedronate.yearly.cost.spine")),
     textOutput("label.Ibandronate.yearly.cost.spine"),
tags$h5(uiOutput("Ibandronate.yearly.cost.spine")),
     textOutput("label.Raloxifene.yearly.cost.spine"),
tags$h5(uiOutput("Raloxifene.yearly.cost.spine")),
     textOutput("label.Strontium.yearly.cost.spine"),
tags$h5(uiOutput("Strontium.yearly.cost.spine")),
     textOutput("label.Denosumab.yearly.cost.spine"),
tags$h5(uiOutput("Denosumab.yearly.cost.spine")),
   textOutput("label.Zoledronate.yearly.cost.spine"),
tags$h5(uiOutput("Zoledronate.yearly.cost.spine")),
   textOutput("label.Teriparatide.yearly.cost.spine"),
tags$h5(uiOutput("Teriparatide.yearly.cost.spine")),
   textOutput("label.Abaloparatide.yearly.cost.spine"),
tags$h5(uiOutput("Abaloparatide.yearly.cost.spine")),
   textOutput("label.Romo.yearly.cost.spine"),
tags$h5(uiOutput("Romo.yearly.cost.spine")),
  tags$h4("After other fracture"),
   textOutput("label.Alendronate.yearly.cost.other"),
tags$h5(uiOutput("Alendronate.yearly.cost.other")),
   textOutput("label.Risedronate.yearly.cost.other"),
tags$h5(uiOutput("Risedronate.yearly.cost.other")),
     textOutput("label.Ibandronate.yearly.cost.other"),
tags$h5(uiOutput("Ibandronate.yearly.cost.other")),
     textOutput("label.Raloxifene.yearly.cost.other"),
tags$h5(uiOutput("Raloxifene.yearly.cost.other")),
     textOutput("label.Strontium.yearly.cost.other"),
tags$h5(uiOutput("Strontium.yearly.cost.other")),
     textOutput("label.Denosumab.yearly.cost.other"),
tags$h5(uiOutput("Denosumab.yearly.cost.other")),
   textOutput("label.Zoledronate.yearly.cost.other"),
tags$h5(uiOutput("Zoledronate.yearly.cost.other")),
   textOutput("label.Teriparatide.yearly.cost.other"),
tags$h5(uiOutput("Teriparatide.yearly.cost.other")),
   textOutput("label.Abaloparatide.yearly.cost.other"),
tags$h5(uiOutput("Abaloparatide.yearly.cost.other")),
   textOutput("label.Romo.yearly.cost.other"),
tags$h5(uiOutput("Romo.yearly.cost.other"))
  )),

  tabPanel("Fracture prevention support: IT support costs",
    mainPanel(
       tags$h3("Fracture prevention support: IT support costs for FLS"),
     
   textOutput("label.fls.database.monthly.maintenance.cost"),
tags$h5(uiOutput("fls.database.monthly.maintenance.cost")),
   textOutput("label.fls.database.installation.cost"),
tags$h5(uiOutput("fls.database.installation.cost")),
   textOutput("label.software.monthly.maintenance.cost"),
tags$h5(uiOutput("software.monthly.maintenance.cost")),
   textOutput("label.software.installation.cost"),
tags$h5(uiOutput("software.installation.cost"))
  )),

 tabPanel("Discharge: discharge destination",
    mainPanel(
tags$h3("Discharge: discharge destination"),
tags$h4("Hip fracture"),
   textOutput("label.prop.discharged.temp.rehab.hip"),
tags$h5(uiOutput("prop.discharged.temp.rehab.hip")),
   textOutput("label.prop.discharged.home.no.support.hip"),
tags$h5(uiOutput("prop.discharged.home.no.support.hip")),
   textOutput("label.prop.discharged.home.support.hip"),
tags$h5(uiOutput("prop.discharged.home.support.hip")),
   textOutput("label.prop.discharged.family.home.hip"),
tags$h5(uiOutput("prop.discharged.family.home.hip")),
   textOutput("label.prop.discharged.long.term.care.hip"),
tags$h5(uiOutput("prop.discharged.long.term.care.hip")),

tags$h4("Spine fracture"),
   textOutput("label.prop.discharged.temp.rehab.spine"),
tags$h5(uiOutput("prop.discharged.temp.rehab.spine")),
   textOutput("label.prop.discharged.home.no.support.spine"),
tags$h5(uiOutput("prop.discharged.home.no.support.spine")),
   textOutput("label.prop.discharged.home.support.spine"),
tags$h5(uiOutput("prop.discharged.home.support.spine")),
   textOutput("label.prop.discharged.family.home.spine"),
tags$h5(uiOutput("prop.discharged.family.home.spine")),
   textOutput("label.prop.discharged.long.term.care.spine"),
tags$h5(uiOutput("prop.discharged.long.term.care.spine")),

tags$h4("Other fracture"),
   textOutput("label.prop.discharged.temp.rehab.other"),
tags$h5(uiOutput("prop.discharged.temp.rehab.other")),
   textOutput("label.prop.discharged.home.no.support.other"),
tags$h5(uiOutput("prop.discharged.home.no.support.other")),
   textOutput("label.prop.discharged.home.support.other"),
tags$h5(uiOutput("prop.discharged.home.support.other")),
   textOutput("label.prop.discharged.family.home.other"),
tags$h5(uiOutput("prop.discharged.family.home.other")),
   textOutput("label.prop.discharged.long.term.care.other"),
tags$h5(uiOutput("prop.discharged.long.term.care.other"))
    )),

 tabPanel("Discharge: temporary rehabilitation",
    mainPanel(
     tags$h3("Discharge: temporary rehabilitation"),
     tags$h4("Length of stay"),
   textOutput("label.temp.rehab.los.hip"),
tags$h5(uiOutput("temp.rehab.los.hip")),
   textOutput("label.temp.rehab.los.spine"),
tags$h5(uiOutput("temp.rehab.los.spine")),
   textOutput("label.temp.rehab.los.other"),
tags$h5(uiOutput("temp.rehab.los.other")), 
  tags$hr(),
   tags$h4("Destination after temporary rehabilitation: hip fracture"),

  textOutput("label.prop.rehab.to.home.no.support.hip"),
tags$h5(uiOutput("prop.rehab.to.home.no.support.hip")), 
  textOutput("label.prop.rehab.to.home.support.hip"),
tags$h5(uiOutput("prop.rehab.to.home.support.hip")), 
  textOutput("label.prop.rehab.to.family.home.hip"),
tags$h5(uiOutput("prop.rehab.to.family.home.hip")), 
  textOutput("label.prop.rehab.to.long.term.care.hip"),
tags$h5(uiOutput("prop.rehab.to.long.term.care.hip")), 

    tags$h4("Destination after temporary rehabilitation: spine fracture"),
  textOutput("label.prop.rehab.to.home.no.support.spine"),
tags$h5(uiOutput("prop.rehab.to.home.no.support.spine")), 
  textOutput("label.prop.rehab.to.home.support.spine"),
tags$h5(uiOutput("prop.rehab.to.home.support.spine")), 
  textOutput("label.prop.rehab.to.family.home.spine"),
tags$h5(uiOutput("prop.rehab.to.family.home.spine")), 
  textOutput("label.prop.rehab.to.long.term.care.spine"),
tags$h5(uiOutput("prop.rehab.to.long.term.care.spine")), 

    tags$h4("Destination after temporary rehabilitation: other fracture"),
  textOutput("label.prop.rehab.to.home.no.support.other"),
tags$h5(uiOutput("prop.rehab.to.home.no.support.other")), 
  textOutput("label.prop.rehab.to.home.support.other"),
tags$h5(uiOutput("prop.rehab.to.home.support.other")), 
  textOutput("label.prop.rehab.to.family.home.other"),
tags$h5(uiOutput("prop.rehab.to.family.home.other")), 
  textOutput("label.prop.rehab.to.long.term.care.other"),
tags$h5(uiOutput("prop.rehab.to.long.term.care.other"))

    )),

 tabPanel("Discharge: clinic (outpatient) vists",
    mainPanel(

    tags$h3("Average number of clinic (outpatient) visits"),
     tags$h4("If hospitalised"),
  textOutput("label.clinic.visits.admitted.spine"),
tags$h5(uiOutput("clinic.visits.admitted.spine")), 
  textOutput("label.clinic.visits.admitted.hip"),
tags$h5(uiOutput("clinic.visits.admitted.hip")), 
  textOutput("label.clinic.visits.admitted.other"),
tags$h5(uiOutput("clinic.visits.admitted.other")), 
    
tags$h4("If not hospitalised"),
  textOutput("label.clinic.visits.not.admitted.spine"),
tags$h5(uiOutput("clinic.visits.not.admitted.spine")), 
  textOutput("label.clinic.visits.not.admitted.hip"),
tags$h5(uiOutput("clinic.visits.not.admitted.hip")), 
  textOutput("label.clinic.visits.not.admitted.other"),
tags$h5(uiOutput("clinic.visits.not.admitted.other"))
    )),

 tabPanel("Costs after discharge",
    mainPanel(
          tags$h3("Costs after discharge"),
  textOutput("label.temp.rehab.daily.cost"),
tags$h5(uiOutput("temp.rehab.daily.cost")), 
  textOutput("label.long.term.care.monthly.cost"),
tags$h5(uiOutput("long.term.care.monthly.cost")), 
  textOutput("label.care.home.monthly.cost"),
tags$h5(uiOutput("care.home.monthly.cost")), 
  textOutput("label.clinic.visit.cost"),
tags$h5(uiOutput("clinic.visit.cost"))
          
    ))


)),


# Run the model -----
"Results", 
## Results page ------ 
tabPanel("Study population",
    mainPanel(
      tags$h3("Study population"), 
#     tableOutput("study_pop_n") , 
htmlOutput("table.summary.microsim.pop.age.sex.fx") %>% withSpinner()
  )), 
tabPanel("Transition probabilities",
    mainPanel(
       tags$h3("Transition probabilities"), 
       tags$h4("Transition probabilities in the absence of medication."), 
  awesomeRadio(
   inputId = "input.tp.plot.gender",
   label = "Gender",
    choices = c("Male", "Female"),
   selected = "Male",
   inline=TRUE
),
awesomeRadio(
   inputId = "input.tp.plot.fx",
   label = "Index fracture",
    choices = c("Hip", "Spine", "Other"),
   selected = "Hip",
   inline=TRUE
),
 plotOutput("plot.tps") %>% withSpinner()
    )),
tabPanel("Export model inputs",
         mainPanel(
           tags$h3("Export model inputs"),
           actionButton("download_model_inputs", "Download"),
           textOutput("download_model_inputs_text")
         )),
tabPanel("Results",
headerPanel(    
  actionButton("run_the_model", "Run the model")),

  div(id = "main",
      tabsetPanel(type = "tabs",
 
     tabPanel("Identified",
  mainPanel(
      awesomeRadio(
   inputId = "choose.identified.plot",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE),
    uiOutput("identified.plot.ui") %>% withSpinner() 
    #  plotOutput("identified.plot") %>% withSpinner() 
  )),              
          tabPanel("Treated",
  mainPanel(
      awesomeRadio(
   inputId = "choose.treat.plot",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE),
      uiOutput("treat.plot.ui") %>% withSpinner() 
  )),                
                  
 tabPanel("Medications",
  mainPanel(
      awesomeRadio(
   inputId = "choose.medication.plot",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE),
      uiOutput("medication.plot.ui") %>% withSpinner() 
  )),
  tabPanel("Adhering",
  mainPanel(
      awesomeRadio(
   inputId = "choose.adhering.plot",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE),
      uiOutput("adhering.plot.ui") %>% withSpinner() 
  )),    
      tabPanel("Relative risk applied",
     mainPanel(
      awesomeRadio(
   inputId = "choose.apply.rr.plot",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE), 
       uiOutput("apply.rr.plot.ui") %>% withSpinner()# ,
    # plotOutput("tps.apply.rr.plot") %>% withSpinner() ,
     #   htmlOutput("table.rr.applied") %>% withSpinner()
    #   plotOutput("rr.applied.plot") %>% withSpinner()
     )),             
 
     tabPanel("Health outcomes",
  tabsetPanel(type = "pills",
     tabPanel("Number of subsequent fractures",
  mainPanel(
tags$h3("Total number of subsequent fractures"), 
sliderTextInput(
   inputId = "sec.frac.time",
   label = "Time in months since index fracture", 
    choices = as.character(seq(1:60)),
  selected = "60"),

  awesomeRadio(
   inputId = "choose.sec.frac.summary",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE
),
htmlOutput("sec.frac.summary") %>% withSpinner()#,
# tags$hr(),
# tags$h3("Difference in proportion of patients by number of re-fractures (FLS - Current practice)"),
# plotOutput("plot.hist.subs.fx") %>% withSpinner()
)),
  tabPanel("Cumulative incidence of subsequent fracture",
  mainPanel(
tags$h3("Cumulative incidence of subsequent fracture"), 
  awesomeRadio(
   inputId = "choose.sec.frac.summary.c.inc",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE
),
plotOutput("plot.c.inc.sec.fx") %>% withSpinner()
)),

  tabPanel("Cumulative incidence of death",
  mainPanel(
tags$h3("Deaths"), 
    sliderTextInput(
   inputId = "deaths.time",
   label = "Time in months since index fracture", 
    choices = as.character(seq(1:60)),
  selected = "60"),

  awesomeRadio(
   inputId = "choose.deaths.summary",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE
),

#tags$h4("Overall"), 
#htmlOutput("deaths.summary") %>% withSpinner()  ,
tags$h3("Cumulative events of death"),
plotOutput("plot.c.inc.deaths") %>% withSpinner()
)),




  tabPanel("MC error",
  mainPanel(
tags$h3("Monte Carlo error"),
a(id = "toggle_plot_mc.error.sec.frac",
"View MC error"),
 shinyjs::hidden(
 tags$div(id = 'plot_mc.error.sec.frac',
  plotOutput("mc.error.sec.frac") %>% withSpinner()
  ))#,
# tags$hr()
))

)),
#      tabPanel("Deaths",
#   mainPanel(
# tags$h3("Deaths"), 
#     sliderTextInput(
#    inputId = "deaths.time",
#    label = "Time in months since index fracture", 
#     choices = as.character(seq(1:60)),
#   selected = "60"),
# 
#   awesomeRadio(
#    inputId = "choose.deaths.summary",
#    label = "",
#     choices = c("Overall",
#                 "By sentinel fracture",
#                 "By sex",
#                 "By sentinel fracture and sex"),
#    selected = "Overall",
#    inline=TRUE
# ),
# 
# #tags$h4("Overall"), 
# #htmlOutput("deaths.summary") %>% withSpinner()  ,
# tags$h3("Cumulative events of death"),
# plotOutput("plot.c.inc.deaths") %>% withSpinner(),
# 
# a(id = "toggle_plot_mc.error.deaths",
# "View MC error"),
#  shinyjs::hidden(
#  tags$div(id = 'plot_mc.error.deaths',
#   plotOutput("mc.error.deaths")
#   ))
# )),

tabPanel("Resource use",
  mainPanel(
tags$h3("Resource use"), 
sliderTextInput(
   inputId = "hcru.time",
   label = "Time in months since index fracture", 
    choices = as.character(seq(1:60)),
  selected = "60"),
 awesomeRadio(
   inputId = "choose.hcru.summary",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE
),

tags$h3("Number of surgeries"),
htmlOutput("summary.procedures") %>% withSpinner(),
tags$h3("Hospital bed days"),
htmlOutput("hosp.los.summary") %>% withSpinner()
)),
     tabPanel("Costs",
  mainPanel(
tags$h3("Costs"), 
sliderTextInput(
   inputId = "costs.time",
   label = "Time in months since index fracture", 
    choices = as.character(seq(1:60)),
  selected = "60"),

  awesomeRadio(
   inputId = "choose.costs.summary",
   label = "",
    choices = c("Overall",
                "By sentinel fracture",
                "By sex",
                "By sentinel fracture and sex"),
   selected = "Overall",
   inline=TRUE
),
tags$h3("Total costs (excluding formal care)"),
htmlOutput("total.cost.excl.location.summary") %>% withSpinner(),
tags$h3("Total costs (including formal care)"),
htmlOutput("total.cost.summary") %>% withSpinner(),
tags$hr(),
tags$h3("Surgical procedure costs"),
htmlOutput("procedure.cost.summary") %>% withSpinner(),
tags$h3("Hospital bed days costs"), 
htmlOutput("hosp.cost.summary") %>% withSpinner(),
tags$h3("Community healthcare costs"),
htmlOutput("comm.cost.summary") %>% withSpinner(),
tags$h3("Hospital outpatient costs"),
htmlOutput("clinic.cost.summary") %>% withSpinner(),
tags$h3("Temporary rehab costs"),
htmlOutput("temp.rehab.cost.summary") %>% withSpinner(),
tags$h3("Formal care costs"),
htmlOutput("location.cost.summary") %>% withSpinner(),
tags$h3("Discharge clinic costs"),
htmlOutput("discharge.clinic.cost.summary") %>% withSpinner(),
tags$h3("Medication costs"),
htmlOutput("medication.cost.summary") %>% withSpinner(),
tags$h3("Fracture prevention staff costs"),
htmlOutput("fx_prev.staff.cost.summary") %>% withSpinner(),
tags$h3("Lab test costs"),
htmlOutput("lab.test.cost.summary") %>% withSpinner(),
tags$h3("DXA costs"),
htmlOutput("dxa.cost.summary") %>% withSpinner()


))
   
                  )) %>% shinyjs::hidden()
                  




                  ) #,
 # tabPanel("m.tr",
 #  mainPanel(
 #    dataTableOutput("table.m.TR") %>% withSpinner()
 #   ))

))
#### SERVER ------
server <- function(input, output,session) {
  
  
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })
## UI text -----
# generate labels for all inputs ----
lapply(1:length(vars$name), function(i) {
    output[[paste0('label.', vars$name[i])]] <- renderText({
      vars$Description[i]
    })
  })
# general text chuncks -----
lapply(1:length(vars$name), function(i) {
    output[[paste0(UI.text$name[i])]] <- renderText({
      UI.text$text[i]
    })
  })




## check boxes -----
 # fx
    observeEvent(input$checkbox_index_fx_option_1,{
      if(input$checkbox_index_fx_option_1 == TRUE) {
      updateCheckboxInput(session, 
                          "checkbox_index_fx_option_2", value = FALSE)
        updateCheckboxInput(session, 
                          "checkbox_index_fx_option_3", value = FALSE)
        } }, ignoreInit = TRUE)
  
  
      observeEvent(input$checkbox_index_fx_option_2,{
      if(input$checkbox_index_fx_option_2 == TRUE) {
      updateCheckboxInput(session, 
                          "checkbox_index_fx_option_1", value = FALSE)
        updateCheckboxInput(session, 
                          "checkbox_index_fx_option_3", value = FALSE)
        } }, ignoreInit = TRUE)
      
            observeEvent(input$checkbox_index_fx_option_3,{
      if(input$checkbox_index_fx_option_3 == TRUE) {
      updateCheckboxInput(session, 
                          "checkbox_index_fx_option_1", value = FALSE)
        updateCheckboxInput(session, 
                          "checkbox_index_fx_option_2", value = FALSE)
        } }, ignoreInit = TRUE)
            
            
            
    
#refx
     observeEvent(input$checkbox_index_refx_5y,{
      if(input$checkbox_index_refx_5y == TRUE) {
      updateCheckboxInput(session, 
                          "checkbox_index_refx_10y", value = FALSE)
        } }, ignoreInit = TRUE)
  
     observeEvent(input$checkbox_index_refx_10y,{
      if(input$checkbox_index_refx_10y == TRUE) {
      updateCheckboxInput(session, 
                          "checkbox_index_refx_5y", value = FALSE)
        } }, ignoreInit = TRUE)
         

## COLLAPSIBLE SECTIONS -----  
shinyjs::onclick("toggle.specify_all_fx",
                  shinyjs::toggle(id = "specify_all_fx", 
                                  anim = TRUE))
   
shinyjs::onclick("toggle.specify_hip_fx_only",
                  shinyjs::toggle(id = "specify_hip_fx_only", 
                                  anim = TRUE))
  
shinyjs::onclick("toggle.specify_pop_szie",
                  shinyjs::toggle(id = "specify_pop_size", 
                                  anim = TRUE))  

shinyjs::onclick("toggle.specify_pop_age",
                  shinyjs::toggle(id = "specify_pop_age", 
                                  anim = TRUE))  


shinyjs::onclick("toggle.specify.5y_refracture",
                  shinyjs::toggle(id = "specify.5y_refracture", 
                                  anim = TRUE))  

shinyjs::onclick("toggle.specify.10y_refracture",
                  shinyjs::toggle(id = "specify.10y_refracture", 
                                  anim = TRUE))  

shinyjs::onclick("toggle.specify.death",
                  shinyjs::toggle(id = "specify.death", 
                                  anim = TRUE))  

shinyjs::onclick("toggle.iden_no_fls_text",
                  shinyjs::toggle(id = "iden_no_fls_text", 
                                  anim = TRUE))  


shinyjs::onclick("toggle.iden_fls_text",
                  shinyjs::toggle(id = "iden_fls_text", 
                                  anim = TRUE))  


shinyjs::onclick("toggle.time_no_fls_text",
                  shinyjs::toggle(id = "time_no_fls_text", 
                                  anim = TRUE)) 

shinyjs::onclick("toggle.time_fls_text",
                  shinyjs::toggle(id = "time_fls_text", 
                                  anim = TRUE)) 


shinyjs::onclick("toggle.meds_no_fls",
                  shinyjs::toggle(id = "meds_no_fls", 
                                  anim = TRUE)) 
shinyjs::onclick("toggle.meds_fls",
                  shinyjs::toggle(id = "meds_fls", 
                                  anim = TRUE)) 

shinyjs::onclick("toggle.romo.meds_no_fls",
                  shinyjs::toggle(id = "romo.meds_no_fls", 
                                  anim = TRUE)) 
shinyjs::onclick("toggle.romo.meds_fls",
                  shinyjs::toggle(id = "romo.meds_fls", 
                                  anim = TRUE)) 

shinyjs::onclick("toggle.abaloparatide.meds_no_fls",
                  shinyjs::toggle(id = "abaloparatide.meds_no_fls", 
                                  anim = TRUE)) 
shinyjs::onclick("toggle.abaloparatide.meds_fls",
                  shinyjs::toggle(id = "abaloparatide.meds_fls", 
                                  anim = TRUE)) 


shinyjs::onclick("toggle.teriparatide.meds_no_fls",
                  shinyjs::toggle(id = "teriparatide.meds_no_fls", 
                                  anim = TRUE)) 
shinyjs::onclick("toggle.teriparatide.meds_fls",
                  shinyjs::toggle(id = "teriparatide.meds_fls", 
                                  anim = TRUE)) 


shinyjs::onclick("toggle.no.fls.monitored",
                  shinyjs::toggle(id = "no.fls.monitored", 
                                  anim = TRUE)) 
shinyjs::onclick("toggle.fls.monitored",
                  shinyjs::toggle(id = "fls.monitored", 
                                  anim = TRUE)) 
           
 
shinyjs::onclick("toggle.primary.adh_no_fls",
                  shinyjs::toggle(id = "primary.adh_no_fls", 
                                  anim = TRUE)) 
shinyjs::onclick("toggle.primary.adh_fls",
                  shinyjs::toggle(id = "primary.adh_fls", 
                                  anim = TRUE)) 

shinyjs::onclick("toggle.monitored.adh.first.year",
                  shinyjs::toggle(id = "monitored.adh.first.year", 
                                  anim = TRUE)) 
shinyjs::onclick("toggle.not.monitored.adh.first.year",
                  shinyjs::toggle(id = "not.monitored.adh.first.year", 
                                  anim = TRUE)) 

shinyjs::onclick("toggle.no.fls.adh.second.year.on",
                  shinyjs::toggle(id = "no.fls.adh.second.year.on", 
                                  anim = TRUE)) 
shinyjs::onclick("toggle.fls.adh.second.year.on",
                  shinyjs::toggle(id = "fls.adh.second.year.on", 
                                  anim = TRUE)) 

shinyjs::onclick("toggle_plot_tps",
                  shinyjs::toggle(id = "plot_tps", 
                                  anim = TRUE)) 


# toggle results####
# Toggling visability of main on button click.
    observeEvent(input$run_the_model, {
      shinyjs::toggle("main")
    })
shinyjs::onclick("toggle_plot_mc.error.sec.frac",
                  shinyjs::toggle(id = "plot_mc.error.sec.frac", anim = TRUE))

shinyjs::onclick("toggle_plot_mc.error.deaths",
                  shinyjs::toggle(id = "plot_mc.error.deaths", anim = TRUE))

## country and region dropdown -----
# based on choice of country

output$country.name <- renderUI({ 
      country.name<-  selectInput("country.name", 
                            tags$h4("Country"), 
                            choices = c("",
                                        "Generic country",
                                        "Colombia",
                                        "France",
                                        "Spain", 
                                        "Mexico", "Brazil",
                                        "Japan", 
                                        "Japan (inject only)",
                                        "Japan (reduced PFC)",
                                        "United Kingdom", "Netherlands",
                                        "Netherlands (perfect PFC)")#,selected = "Spain"
                            )
  

  })

output$region.selection <- renderUI({
          #  req(input$country.name)   
        if(input$country.name==""){ 
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c(""))}
  
  
  
         if(input$country.name=="Generic country"){ 
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country"
                                        ))}
  
          if(input$country.name=="Colombia"){ 
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country"
                            ))}
  
  
           if(input$country.name=="France"){ 
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country"
                            ))}
  
           if(input$country.name=="Mexico"){ 
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country",
                                        "Aguascalientes",
"Baja California","Baja California Sur","Campeche",
"Coahuila de Zaragoza",
"Colima","Chiapas","Chihuahua","Ciudad de México","Durango",
"Guanajuato","Guerrero","Hidalgo","Jalisco",
"Estado de México",
"Michoacán de Ocampo",
"Morelos","Nayarit","Nuevo León",
"Oaxaca","Puebla","Querétaro",
"Quintana Roo","San Luis Potosí",
"Sinaloa","Sonora","Tabasco","Tamaulipas",
"Tlaxcala","Veracruz de Ignacio de la Llave",
"Yucatán","Zacatecas"
                            ))}
  
           if(input$country.name=="Brazil"){ 
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country"
                                        ))}
  
  
      if(input$country.name=="Spain"){ #autonomous communities
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country",
"Andalusia","Aragon","Asturias","Balearic Islands","Basque Country",
"Canary Islands","Cantabria","Castile-La Mancha","Castile and Leon",
"Catalonia", "Ceuta", 
"Extremadura","Galicia","La Rioja","Melilla","Community of Madrid",
"Region of Murcia","Navarre","Valencian Community"))}
    
        if(input$country.name %in% c("Japan", "Japan (inject only)", "Japan (reduced PFC)")){ #prefectures
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country",
"Aichi","Akita","Aomori","Chiba",
"Ehime","Fukui","Fukuoka","Fukushima","Gifu","Gunma",
"Hiroshima","Hokkaido","Hyogo","Ibaraki","Ishikawa",
"Iwate","Kagawa","Kagoshima","Kanagawa","Kochi","Kumamoto","Kyoto","Mie",
"Miyagi","Miyazaki","Nagano","Nagasaki","Nara","Niigata",
"Oita","Okayama","Okinawa","Osaka","Saga","Saitama","Shiga",
"Shimane","Shizuoka","Tochigi","Tokushima","Tokyo","Tottori",
"Toyama","Wakayama","Yamagata", "Yamaguchi", "Yamanashi"))}
  
  
       if(input$country.name=="United Kingdom"){ 
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country",
                                        "England",
                                        "England - North East",
"England - North West","England - Yorkshire And The Humber",
"England - East Midlands","England - West Midlands",
"England - East","England - London","England - South East","England - South West",
"Wales","Scotland","Northern Ireland"))}
  
       if(input$country.name=="Netherlands"){ 
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country",
                                        "Drenthe",
"Flevoland","Friesland",
"Gelderland","Groningen",
"Limburg","North Brabant",
"North Holland","Overijssel","South Holland",
"Utrecht","Zeeland"
                                        ))}
        
  if(input$country.name=="Netherlands (perfect PFC)"){ 
    region.selection<-  selectInput("region.name", 
                            tags$h4("Region"), 
                            choices = c("Whole country",
                                        "Drenthe",
"Flevoland","Friesland",
"Gelderland","Groningen",
"Limburg","North Brabant",
"North Holland","Overijssel","South Holland",
"Utrecht","Zeeland"
                                        ))}

    region.selection

        })
  
lapply(c("country.name"),
       function(x) outputOptions(output, x, suspendWhenHidden = FALSE))

lapply(c("region.selection"),
       function(x) outputOptions(output, x, suspendWhenHidden = FALSE))

  

  


get.working.country_data<-reactive({
 
   req(input$country.name)
  
   if(input$country.name=="Generic country"){
    working.country_data<-generic_country.inputs
      }
  
    if(input$country.name=="Colombia"){
    working.country_data<-colombia.inputs
    }
  
    if(input$country.name=="France"){
    working.country_data<-france.inputs
    }
  
    if(input$country.name=="Mexico"){
    working.country_data<-mexico.inputs
    }
  
    if(input$country.name=="Brazil"){
    working.country_data<-brazil.inputs
  }
  
  if(input$country.name=="Spain"){
    working.country_data<-spain.inputs
  }
  
    if(input$country.name=="Japan"){
    working.country_data<-japan.inputs
    }
  if(input$country.name=="Japan (inject only)"){
    working.country_data<-japan_inject_only.inputs
  }
  if(input$country.name=="Japan (reduced PFC)"){
    working.country_data<-Japan_reducedPFC
    }
  
 
  
      if(input$country.name=="United Kingdom"){
    working.country_data<-united_kingdom.inputs
      }
  
      if(input$country.name=="Netherlands"){
    working.country_data<-netherlands.inputs
      }
  
  if(input$country.name=="Netherlands (perfect PFC)"){
    working.country_data<-NetherlandsPerfectPfc.inputs
  }
  
  
  working.country_data
  }) 
  
get.working.country_cost.data<-reactive({
    req(input$country.name)
  
    if(input$country.name=="Generic country"){
    working.country_cost.data<-generic_country.cost.inputs
  }
  
   if(input$country.name=="Colombia"){
    working.country_cost.data<-colombia.cost.inputs
   }
  
   if(input$country.name=="France"){
    working.country_cost.data<-france.cost.inputs
   }
  
   if(input$country.name=="Mexico"){
    working.country_cost.data<-mexico.cost.inputs
   }
  
   if(input$country.name=="Brazil"){
    working.country_cost.data<-brazil.cost.inputs
  }
  
  if(input$country.name=="Spain"){
    working.country_cost.data<-spain.cost.inputs
  }
  
    if(input$country.name=="Japan"){
    working.country_cost.data<-japan.cost.inputs
    }
  if(input$country.name=="Japan (inject only)"){
    working.country_cost.data<-Japan.costs_inject_only
  }
  if(input$country.name=="Japan (reduced PFC)"){
    working.country_cost.data<-Japan.costs_reducedPFC
    }
    
      if(input$country.name=="United Kingdom"){
    working.country_cost.data<-united_kingdom.cost.inputs
      }
  
      if(input$country.name=="Netherlands"){
    working.country_cost.data<-netherlands.cost.inputs
  }
  
    if(input$country.name=="Netherlands (perfect PFC)"){
    working.country_cost.data<-NetherlandsPerfectPfcCosts
  }
  
  working.country_cost.data
  }) 

get.working.country_qol.data<-reactive({
    req(input$country.name)
  
    if(input$country.name=="Generic country"){
    working.country_qol.data<-generic_country.qol.inputs
  }
  
   if(input$country.name=="Colombia"){
    working.country_qol.data<-generic_country.qol.inputs
   }
  
   if(input$country.name=="France"){
    working.country_qol.data<-generic_country.qol.inputs
   }
  
   if(input$country.name=="Mexico"){
    working.country_qol.data<-generic_country.qol.inputs
   }
  
   if(input$country.name=="Brazil"){
    working.country_qol.data<-generic_country.qol.inputs
  }
  
  if(input$country.name=="Spain"){
    working.country_qol.data<-generic_country.qol.inputs
  }
  
    if(input$country.name=="Japan"){
    working.country_qol.data<-generic_country.qol.inputs
    }
  if(input$country.name=="Japan (inject only)"){
    working.country_qol.data<-generic_country.qol.inputs
  }
  if(input$country.name=="Japan (reduced PFC)"){
    working.country_qol.data<-generic_country.qol.inputs
    }
    
      if(input$country.name=="United Kingdom"){
    working.country_qol.data<-generic_country.qol.inputs
      }
  
      if(input$country.name=="Netherlands"){
    working.country_qol.data<-generic_country.qol.inputs
  }
  
    if(input$country.name=="Netherlands (perfect PFC)"){
    working.country_qol.data<-generic_country.qol.inputs
  }
  
  working.country_qol.data
  }) 


### DEFAULTS, USER CAN EDIT (I.E. IN UI)
## general inputs  -------

# prop female ----
output$prop.female<-renderUI({
   numericInput('prop.female', '', 
                value=as.numeric(general.inputs %>% 
    filter(name=="prop.female") %>% 
    select(Value)), 
    step=0.1)
})

# Absolute effect of FLS on adherence  -----
output$ae_FLS_adh <- renderUI({

numericInput('ae_FLS_adh', '', 
                value=  as.numeric(general.inputs %>% 
    filter(name=="ae_FLS_adh") %>% 
    select(Value)),
                step=1)
})


## country specific inputs ------


## population default -----

output$pop_size <- renderUI({
  
 # browser()
  n<-0
  
# whole country estimates  
  
  
  
 if(input$region.name=="Whole country"){
  # browser()
    if(input$country.name=={"Colombia"}){
    n<- as.character(general.inputs %>% 
       filter(name=="n_Colombia") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
    } 
    if(input$country.name=={"France"}){
    n<- as.character(general.inputs %>% 
       filter(name=="n_France") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
    } 
    if(input$country.name=={"Mexico"}){
    n<- as.character(general.inputs %>% 
       filter(name=="n_Mexico") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
    } 
    if(input$country.name=={"Brazil"}){
    n<- as.character(general.inputs %>% 
       filter(name=="n_Brazil") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
   } 
   
   
   if(input$country.name=={"Spain"}){
    n<- as.character(general.inputs %>% 
       filter(name=="n_Spain") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
   } 
   if(input$country.name %in% c("Japan", "Japan (reduced PFC)","Japan (inject only)")){
    n<- as.character(general.inputs %>%  
       filter(name=="n_Japan") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
   } 
   if(input$country.name=={"United Kingdom"}){
    n<- as.character(general.inputs %>% 
       filter(name=="n_United_Kingdom") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
   } 
   if(input$country.name=={"Netherlands"}){
    n<- as.character(general.inputs %>% 
       filter(name=="n_Netherlands") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
   } 
   
      if(input$country.name=={"Generic country"}){
    n<- as.character(general.inputs %>% 
       filter(name=="n_Generic_country") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
   } 
   
      if(input$country.name=={"Netherlands (perfect PFC)"}){
    n<- as.character(general.inputs %>% 
       filter(name=="n_Netherlands") %>% 
       select(Value))
     n<-gsub(",", "", n, fixed = TRUE) 
   } 
 }
  
  
  if(input$region.name!="Whole country"){
  # browser()
    
    if(str_detect(input$region.name, "Baja California")==FALSE){
      
     n<- as.character( general.inputs %>% 
      filter( str_detect(general.inputs$name,
               gsub(" ", "_", input$region.name)
               ) == "TRUE") %>% 
      select(Value))
     
     n<-gsub(",", "", n, fixed = TRUE) 
  }
  
     if(str_detect(input$region.name, "Baja California")==TRUE){
      
     n<- as.character( general.inputs %>% 
      filter( str_detect(general.inputs$name,
               gsub(" ", "_", input$region.name)
               ) == "TRUE") %>% 
        head(1) %>% 
      select(Value))
     
     n<-gsub(",", "", n, fixed = TRUE) 
  } 
  
  
    
    
  }
  
  
   numericInput('pop_size', '',
                value=n,step=50)
  }) 



# age ----
  output$plots_and_radios <- renderUI({
  
    working.country_data<-get.working.country_data()

   numericInput('av_age_hip.male', '',
                value=as.numeric(working.country_data %>%
    filter(name=="av_age_hip.male") %>%
    select(Value)),
    step=0.1)
    
  })




output$av_age_hip.male <- renderUI({
  working.country_data<-get.working.country_data()

   numericInput('av_age_hip.male', '',
                value=as.numeric(working.country_data %>%
    filter(name=="av_age_hip.male") %>%
    select(Value)),
    step=0.1)
})

output$av_age_spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('av_age_spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="av_age_spine.male") %>% 
    select(Value)), 
    step=0.1)
})

output$av_age_other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('av_age_other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="av_age_other.male") %>% 
    select(Value)), 
    step=0.1)
})

output$av_age_hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('av_age_hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="av_age_hip.female") %>% 
    select(Value)), 
    step=0.1)
})

output$av_age_spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('av_age_spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="av_age_spine.female") %>% 
    select(Value)), 
    step=0.1)
})

output$av_age_other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('av_age_other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="av_age_other.female") %>% 
    select(Value)), 
    step=0.1)
})



# other pop ----
output$gen_pop_over_50 <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('gen_pop_over_50', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="gen_pop_over_50") %>% 
    select(Value)), 
    step=0.1)
})

output$over_50.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('over_50.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="over_50.female") %>% 
    select(Value)), 
    step=0.1)
})

output$prob_hip_fx_over_50.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob_hip_fx_over_50.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob_hip_fx_over_50.male") %>% 
    select(Value)), 
    step=0.1)
})

output$prob_hip_fx_over_50.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob_hip_fx_over_50.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob_hip_fx_over_50.female") %>% 
    select(Value)), 
    step=0.1)
})





# Risk of re-fractures -----
output$risk_hip_fracture_after_hip_5y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_hip_5y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_hip_5y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_hip_5y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_hip_5y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_hip_5y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_hip_5y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_hip_5y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_hip_5y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_hip_fracture_after_spine_5y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_spine_5y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_spine_5y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_spine_5y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_spine_5y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_spine_5y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_spine_5y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_spine_5y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_spine_5y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_hip_fracture_after_other_5y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_other_5y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_other_5y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_other_5y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_other_5y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_other_5y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_other_5y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_other_5y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_other_5y.male") %>% 
    select(Value)), 
    step=0.1)
})

output$risk_hip_fracture_after_hip_10y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_hip_10y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_hip_10y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_hip_10y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_hip_10y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_hip_10y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_hip_10y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_hip_10y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_hip_10y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_hip_fracture_after_spine_10y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_spine_10y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_spine_10y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_spine_10y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_spine_10y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_spine_10y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_spine_10y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_spine_10y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_spine_10y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_hip_fracture_after_other_10y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_other_10y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_other_10y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_other_10y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_other_10y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_other_10y.male") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_other_10y.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_other_10y.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_other_10y.male") %>% 
    select(Value)), 
    step=0.1)
})






output$risk_hip_fracture_after_hip_5y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_hip_5y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_hip_5y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_hip_5y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_hip_5y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_hip_5y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_hip_5y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_hip_5y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_hip_5y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_hip_fracture_after_spine_5y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_spine_5y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_spine_5y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_spine_5y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_spine_5y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_spine_5y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_spine_5y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_spine_5y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_spine_5y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_hip_fracture_after_other_5y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_other_5y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_other_5y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_other_5y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_other_5y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_other_5y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_other_5y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_other_5y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_other_5y.female") %>% 
    select(Value)), 
    step=0.1)
})

output$risk_hip_fracture_after_hip_10y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_hip_10y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_hip_10y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_hip_10y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_hip_10y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_hip_10y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_hip_10y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_hip_10y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_hip_10y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_hip_fracture_after_spine_10y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_spine_10y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_spine_10y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_spine_10y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_spine_10y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_spine_10y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_spine_10y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_spine_10y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_spine_10y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_hip_fracture_after_other_10y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_hip_fracture_after_other_10y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_hip_fracture_after_other_10y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_spine_fracture_after_other_10y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_spine_fracture_after_other_10y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_spine_fracture_after_other_10y.female") %>% 
    select(Value)), 
    step=0.1)
})
output$risk_other_fracture_after_other_10y.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('risk_other_fracture_after_other_10y.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="risk_other_fracture_after_other_10y.female") %>% 
    select(Value)), 
    step=0.1)
})





# Treatment practice
output$no.fls.trt.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.alendronate.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.alendronate.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.alendronate.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.risedronate.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.risedronate.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.risedronate.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.strontium.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.strontium.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.strontium.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.ibandronate.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.ibandronate.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.ibandronate.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.raloxifene.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.raloxifene.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.raloxifene.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.denosumab.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.denosumab.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.denosumab.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.zoledronate.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.zoledronate.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.zoledronate.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.teriparatide.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.teriparatide.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.teriparatide.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.abaloparatide.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.abaloparatide.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.abaloparatide.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.romo.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.romo.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.romo.spine.male") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.trt.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.alendronate.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.alendronate.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.alendronate.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.risedronate.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.risedronate.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.risedronate.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.strontium.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.strontium.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.strontium.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.ibandronate.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.ibandronate.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.ibandronate.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.raloxifene.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.raloxifene.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.raloxifene.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.denosumab.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.denosumab.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.denosumab.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.zoledronate.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.zoledronate.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.zoledronate.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.teriparatide.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.teriparatide.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.teriparatide.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.abaloparatide.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.abaloparatide.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.abaloparatide.spine.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.romo.spine.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.romo.spine.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.romo.spine.male") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.trt.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.alendronate.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.alendronate.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.alendronate.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.risedronate.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.risedronate.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.risedronate.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.strontium.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.strontium.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.strontium.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.ibandronate.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.ibandronate.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.ibandronate.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.raloxifene.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.raloxifene.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.raloxifene.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.denosumab.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.denosumab.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.denosumab.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.zoledronate.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.zoledronate.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.zoledronate.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.teriparatide.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.teriparatide.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.teriparatide.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.abaloparatide.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.abaloparatide.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.abaloparatide.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.romo.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.romo.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.romo.hip.male") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.trt.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.alendronate.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.alendronate.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.alendronate.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.risedronate.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.risedronate.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.risedronate.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.strontium.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.strontium.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.strontium.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.ibandronate.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.ibandronate.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.ibandronate.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.raloxifene.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.raloxifene.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.raloxifene.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.denosumab.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.denosumab.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.denosumab.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.zoledronate.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.zoledronate.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.zoledronate.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.teriparatide.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.teriparatide.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.teriparatide.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.abaloparatide.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.abaloparatide.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.abaloparatide.hip.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.romo.hip.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.romo.hip.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.romo.hip.male") %>% 
    select(Value)), 
    step=0.1)
})


output$no.fls.trt.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.alendronate.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.alendronate.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.alendronate.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.risedronate.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.risedronate.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.risedronate.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.strontium.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.strontium.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.strontium.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.ibandronate.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.ibandronate.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.ibandronate.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.raloxifene.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.raloxifene.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.raloxifene.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.denosumab.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.denosumab.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.denosumab.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.zoledronate.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.zoledronate.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.zoledronate.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.teriparatide.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.teriparatide.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.teriparatide.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.abaloparatide.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.abaloparatide.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.abaloparatide.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.romo.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.romo.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.romo.other.male") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.trt.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.alendronate.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.alendronate.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.alendronate.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.risedronate.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.risedronate.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.risedronate.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.strontium.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.strontium.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.strontium.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.ibandronate.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.ibandronate.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.ibandronate.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.raloxifene.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.raloxifene.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.raloxifene.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.denosumab.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.denosumab.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.denosumab.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.zoledronate.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.zoledronate.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.zoledronate.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.teriparatide.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.teriparatide.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.teriparatide.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.abaloparatide.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.abaloparatide.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.abaloparatide.other.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.romo.other.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.romo.other.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.romo.other.male") %>% 
    select(Value)), 
    step=0.1)
})





output$no.fls.trt.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.alendronate.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.alendronate.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.alendronate.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.risedronate.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.risedronate.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.risedronate.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.strontium.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.strontium.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.strontium.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.ibandronate.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.ibandronate.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.ibandronate.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.raloxifene.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.raloxifene.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.raloxifene.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.denosumab.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.denosumab.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.denosumab.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.zoledronate.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.zoledronate.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.zoledronate.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.teriparatide.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.teriparatide.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.teriparatide.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.abaloparatide.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.abaloparatide.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.abaloparatide.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.romo.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.romo.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.romo.spine.female") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.trt.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.alendronate.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.alendronate.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.alendronate.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.risedronate.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.risedronate.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.risedronate.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.strontium.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.strontium.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.strontium.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.ibandronate.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.ibandronate.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.ibandronate.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.raloxifene.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.raloxifene.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.raloxifene.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.denosumab.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.denosumab.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.denosumab.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.zoledronate.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.zoledronate.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.zoledronate.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.teriparatide.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.teriparatide.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.teriparatide.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.abaloparatide.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.abaloparatide.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.abaloparatide.spine.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.romo.spine.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.romo.spine.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.romo.spine.female") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.trt.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.alendronate.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.alendronate.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.alendronate.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.risedronate.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.risedronate.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.risedronate.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.strontium.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.strontium.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.strontium.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.ibandronate.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.ibandronate.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.ibandronate.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.raloxifene.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.raloxifene.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.raloxifene.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.denosumab.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.denosumab.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.denosumab.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.zoledronate.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.zoledronate.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.zoledronate.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.teriparatide.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.teriparatide.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.teriparatide.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.abaloparatide.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.abaloparatide.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.abaloparatide.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.romo.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.romo.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.romo.hip.female") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.trt.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.alendronate.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.alendronate.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.alendronate.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.risedronate.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.risedronate.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.risedronate.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.strontium.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.strontium.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.strontium.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.ibandronate.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.ibandronate.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.ibandronate.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.raloxifene.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.raloxifene.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.raloxifene.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.denosumab.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.denosumab.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.denosumab.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.zoledronate.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.zoledronate.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.zoledronate.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.teriparatide.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.teriparatide.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.teriparatide.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.abaloparatide.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.abaloparatide.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.abaloparatide.hip.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.romo.hip.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.romo.hip.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.romo.hip.female") %>% 
    select(Value)), 
    step=0.1)
})


output$no.fls.trt.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.alendronate.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.alendronate.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.alendronate.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.risedronate.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.risedronate.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.risedronate.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.strontium.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.strontium.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.strontium.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.ibandronate.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.ibandronate.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.ibandronate.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.raloxifene.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.raloxifene.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.raloxifene.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.denosumab.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.denosumab.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.denosumab.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.zoledronate.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.zoledronate.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.zoledronate.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.teriparatide.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.teriparatide.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.teriparatide.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.abaloparatide.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.abaloparatide.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.abaloparatide.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.trt.romo.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.trt.romo.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.trt.romo.other.female") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.trt.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.alendronate.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.alendronate.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.alendronate.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.risedronate.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.risedronate.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.risedronate.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.strontium.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.strontium.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.strontium.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.ibandronate.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.ibandronate.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.ibandronate.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.raloxifene.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.raloxifene.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.raloxifene.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.denosumab.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.denosumab.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.denosumab.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.zoledronate.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.zoledronate.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.zoledronate.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.teriparatide.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.teriparatide.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.teriparatide.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.abaloparatide.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.abaloparatide.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.abaloparatide.other.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.trt.romo.other.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.trt.romo.other.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.trt.romo.other.female") %>% 
    select(Value)), 
    step=0.1)
})



# risk profiles ----
output$prop.fx.low.risk <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prop.fx.low.risk', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.fx.low.risk") %>% 
    select(Value)), 
    step=0.1)
})

output$prop.fx.intermediate.risk <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prop.fx.intermediate.risk', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.fx.intermediate.risk") %>% 
    select(Value)), 
    step=0.1)
})

output$prop.fx.high.risk <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prop.fx.high.risk', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.fx.high.risk") %>% 
    select(Value)), 
    step=0.1)
})



output$fx.multiplier.low.risk <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fx.multiplier.low.risk', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fx.multiplier.low.risk") %>% 
    select(Value)), 
    step=0.1)
})

output$fx.multiplier.intermediate.risk <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fx.multiplier.intermediate.risk', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fx.multiplier.intermediate.risk") %>% 
    select(Value)), 
    step=0.1)
})

output$fx.multiplier.high.risk <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fx.multiplier.high.risk', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fx.multiplier.high.risk") %>% 
    select(Value)), 
    step=0.1)
})


# after romo ---------
output$romo.to.nothing.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.nothing.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.nothing.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.alendronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.alendronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.alendronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.alendronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.alendronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.alendronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.risedronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.risedronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.risedronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.strontium.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.strontium.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.strontium.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.ibandronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.ibandronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.ibandronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.raloxifene.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.raloxifene.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.raloxifene.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.denosumab.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.denosumab.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.denosumab.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.zoledronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.zoledronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.zoledronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.nothing.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.nothing.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.nothing.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$romo.to.alendronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.alendronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.alendronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.alendronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.alendronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.alendronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.risedronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.risedronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.risedronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.strontium.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.strontium.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.strontium.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.ibandronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.ibandronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.ibandronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.raloxifene.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.raloxifene.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.raloxifene.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.denosumab.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.denosumab.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.denosumab.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.zoledronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.zoledronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.zoledronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$romo.to.nothing.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.nothing.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.nothing.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.alendronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.alendronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.alendronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.alendronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.alendronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.alendronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.risedronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.risedronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.risedronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.strontium.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.strontium.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.strontium.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.ibandronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.ibandronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.ibandronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.raloxifene.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.raloxifene.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.raloxifene.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.denosumab.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.denosumab.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.denosumab.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.zoledronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.zoledronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.zoledronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.nothing.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.nothing.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.nothing.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$romo.to.alendronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.alendronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.alendronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.alendronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.alendronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.alendronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.risedronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.risedronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.risedronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.strontium.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.strontium.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.strontium.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.ibandronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.ibandronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.ibandronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.raloxifene.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.raloxifene.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.raloxifene.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.denosumab.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.denosumab.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.denosumab.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$romo.to.zoledronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('romo.to.zoledronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="romo.to.zoledronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})


# after abaloparatide ---------
output$abaloparatide.to.nothing.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.nothing.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.nothing.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.alendronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.alendronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.alendronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.alendronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.alendronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.alendronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.risedronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.risedronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.risedronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.strontium.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.strontium.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.strontium.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.ibandronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.ibandronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.ibandronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.raloxifene.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.raloxifene.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.raloxifene.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.denosumab.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.denosumab.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.denosumab.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.zoledronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.zoledronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.zoledronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.nothing.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.nothing.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.nothing.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$abaloparatide.to.alendronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.alendronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.alendronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.alendronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.alendronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.alendronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.risedronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.risedronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.risedronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.strontium.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.strontium.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.strontium.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.ibandronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.ibandronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.ibandronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.raloxifene.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.raloxifene.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.raloxifene.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.denosumab.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.denosumab.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.denosumab.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.zoledronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.zoledronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.zoledronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$abaloparatide.to.nothing.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.nothing.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.nothing.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.alendronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.alendronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.alendronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.alendronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.alendronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.alendronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.risedronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.risedronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.risedronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.strontium.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.strontium.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.strontium.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.ibandronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.ibandronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.ibandronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.raloxifene.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.raloxifene.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.raloxifene.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.denosumab.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.denosumab.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.denosumab.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.zoledronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.zoledronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.zoledronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.nothing.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.nothing.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.nothing.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$abaloparatide.to.alendronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.alendronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.alendronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.alendronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.alendronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.alendronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.risedronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.risedronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.risedronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.strontium.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.strontium.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.strontium.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.ibandronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.ibandronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.ibandronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.raloxifene.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.raloxifene.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.raloxifene.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.denosumab.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.denosumab.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.denosumab.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$abaloparatide.to.zoledronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('abaloparatide.to.zoledronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="abaloparatide.to.zoledronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})



# after teriparatide ---------
output$teriparatide.to.nothing.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.nothing.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.nothing.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.alendronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.alendronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.alendronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.alendronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.alendronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.alendronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.risedronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.risedronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.risedronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.strontium.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.strontium.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.strontium.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.ibandronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.ibandronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.ibandronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.raloxifene.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.raloxifene.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.raloxifene.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.denosumab.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.denosumab.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.denosumab.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.zoledronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.zoledronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.zoledronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.nothing.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.nothing.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.nothing.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$teriparatide.to.alendronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.alendronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.alendronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.alendronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.alendronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.alendronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.risedronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.risedronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.risedronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.strontium.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.strontium.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.strontium.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.ibandronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.ibandronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.ibandronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.raloxifene.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.raloxifene.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.raloxifene.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.denosumab.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.denosumab.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.denosumab.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.zoledronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.zoledronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.zoledronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$teriparatide.to.nothing.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.nothing.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.nothing.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.alendronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.alendronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.alendronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.alendronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.alendronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.alendronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.risedronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.risedronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.risedronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.strontium.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.strontium.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.strontium.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.ibandronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.ibandronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.ibandronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.raloxifene.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.raloxifene.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.raloxifene.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.denosumab.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.denosumab.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.denosumab.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.zoledronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.zoledronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.zoledronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.nothing.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.nothing.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.nothing.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$teriparatide.to.alendronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.alendronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.alendronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.alendronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.alendronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.alendronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.risedronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.risedronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.risedronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.strontium.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.strontium.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.strontium.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.ibandronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.ibandronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.ibandronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.raloxifene.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.raloxifene.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.raloxifene.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.denosumab.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.denosumab.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.denosumab.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$teriparatide.to.zoledronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('teriparatide.to.zoledronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="teriparatide.to.zoledronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})




# prob identification ----
output$no.fls.prob_identification.hip <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.prob_identification.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.prob_identification.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.prob_identification.spine <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.prob_identification.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.prob_identification.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.prob_identification.other <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.prob_identification.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.prob_identification.other") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.prob_identification.hip <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.prob_identification.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.prob_identification.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.prob_identification.spine <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.prob_identification.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.prob_identification.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.prob_identification.other <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.prob_identification.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.prob_identification.other") %>% 
    select(Value)), 
    step=0.1)
})



# time to treat ----
output$no.fls.time_to_treat.hip <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.time_to_treat.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.time_to_treat.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.time_to_treat.spine <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.time_to_treat.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.time_to_treat.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.time_to_treat.other <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.time_to_treat.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.time_to_treat.other") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.time_to_treat.hip <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.time_to_treat.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.time_to_treat.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.time_to_treat.spine <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.time_to_treat.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.time_to_treat.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.time_to_treat.other <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.time_to_treat.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.time_to_treat.other") %>% 
    select(Value)), 
    step=0.1)
})



# monitoring -----
output$no.fls.monitored.spine.4m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.spine.4m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.spine.4m.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.monitored.hip.4m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.hip.4m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.hip.4m.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.monitored.other.4m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.other.4m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.other.4m.male") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.monitored.spine.12m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.spine.12m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.spine.12m.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.monitored.hip.12m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.hip.12m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.hip.12m.male") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.monitored.other.12m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.other.12m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.other.12m.male") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.monitored.spine.4m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.spine.4m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.spine.4m.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.monitored.hip.4m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.hip.4m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.hip.4m.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.monitored.other.4m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.other.4m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.other.4m.female") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.monitored.spine.12m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.spine.12m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.spine.12m.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.monitored.hip.12m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.hip.12m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.hip.12m.female") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.monitored.other.12m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('no.fls.monitored.other.12m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.monitored.other.12m.female") %>% 
    select(Value)), 
    step=0.1)
})




output$fls.monitored.spine.4m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.spine.4m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.spine.4m.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.monitored.hip.4m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.hip.4m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.hip.4m.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.monitored.other.4m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.other.4m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.other.4m.male") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.monitored.spine.12m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.spine.12m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.spine.12m.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.monitored.hip.12m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.hip.12m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.hip.12m.male") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.monitored.other.12m.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.other.12m.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.other.12m.male") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.monitored.spine.4m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.spine.4m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.spine.4m.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.monitored.hip.4m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.hip.4m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.hip.4m.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.monitored.other.4m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.other.4m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.other.4m.female") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.monitored.spine.12m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.spine.12m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.spine.12m.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.monitored.hip.12m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.hip.12m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.hip.12m.female") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.monitored.other.12m.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('fls.monitored.other.12m.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.monitored.other.12m.female") %>% 
    select(Value)), 
    step=0.1)
})



# primary adherence ----
output$primary.adh_alendronate.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.spine.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.spine.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.spine.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.hip.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.hip.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.hip.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.other.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.other.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.other.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.spine.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.spine.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.spine.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.hip.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.hip.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.hip.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.other.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.other.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.other.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})






output$primary.adh_alendronate.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.spine.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.spine.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.spine.fls.male") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.hip.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.hip.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.hip.fls.male") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.other.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.other.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.other.fls.male") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.spine.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.spine.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.spine.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.hip.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.hip.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.hip.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$primary.adh_alendronate.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_alendronate.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_alendronate.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_risedronate.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_risedronate.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_risedronate.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_strontium.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_strontium.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_strontium.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_ibandronate.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_ibandronate.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_ibandronate.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_raloxifene.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_raloxifene.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_raloxifene.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_denosumab.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_denosumab.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_denosumab.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_zoledronate.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_zoledronate.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_zoledronate.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_teriparatide.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_teriparatide.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_teriparatide.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_abaloparatide.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_abaloparatide.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_abaloparatide.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$primary.adh_romo.other.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('primary.adh_romo.other.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="primary.adh_romo.other.fls.female") %>% 
    select(Value)), 
    step=0.1)
})





# monitoring - adherence ----
output$not.monitored.4m_adh_alendronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_alendronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_alendronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_risedronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_risedronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_risedronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_strontium.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_strontium.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_strontium.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_ibandronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_ibandronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_ibandronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_raloxifene.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_raloxifene.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_raloxifene.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_denosumab.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_denosumab.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_denosumab.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_zoledronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_zoledronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_zoledronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_teriparatide.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_teriparatide.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_teriparatide.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_abaloparatide.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_abaloparatide.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_abaloparatide.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_romo.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_romo.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_romo.male") %>% 
    select(Value)), 
    step=0.1)
})

output$not.monitored.12m_adh_alendronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_alendronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_alendronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_risedronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_risedronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_risedronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_strontium.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_strontium.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_strontium.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_ibandronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_ibandronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_ibandronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_raloxifene.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_raloxifene.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_raloxifene.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_denosumab.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_denosumab.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_denosumab.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_zoledronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_zoledronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_zoledronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_teriparatide.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_teriparatide.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_teriparatide.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_abaloparatide.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_abaloparatide.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_abaloparatide.male") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_romo.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_romo.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_romo.male") %>% 
    select(Value)), 
    step=0.1)
})


output$not.monitored.4m_adh_alendronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_alendronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_alendronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_risedronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_risedronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_risedronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_strontium.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_strontium.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_strontium.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_ibandronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_ibandronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_ibandronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_raloxifene.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_raloxifene.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_raloxifene.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_denosumab.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_denosumab.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_denosumab.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_zoledronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_zoledronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_zoledronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_teriparatide.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_teriparatide.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_teriparatide.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_abaloparatide.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_abaloparatide.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_abaloparatide.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.4m_adh_romo.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.4m_adh_romo.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.4m_adh_romo.female") %>% 
    select(Value)), 
    step=0.1)
})

output$not.monitored.12m_adh_alendronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_alendronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_alendronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_risedronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_risedronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_risedronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_strontium.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_strontium.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_strontium.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_ibandronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_ibandronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_ibandronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_raloxifene.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_raloxifene.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_raloxifene.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_denosumab.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_denosumab.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_denosumab.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_zoledronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_zoledronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_zoledronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_teriparatide.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_teriparatide.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_teriparatide.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_abaloparatide.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_abaloparatide.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_abaloparatide.female") %>% 
    select(Value)), 
    step=0.1)
})
output$not.monitored.12m_adh_romo.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('not.monitored.12m_adh_romo.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="not.monitored.12m_adh_romo.female") %>% 
    select(Value)), 
    step=0.1)
})











output$monitored.4m_adh_alendronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_alendronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_alendronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_risedronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_risedronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_risedronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_strontium.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_strontium.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_strontium.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_ibandronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_ibandronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_ibandronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_raloxifene.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_raloxifene.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_raloxifene.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_denosumab.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_denosumab.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_denosumab.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_zoledronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_zoledronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_zoledronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_teriparatide.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_teriparatide.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_teriparatide.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_abaloparatide.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_abaloparatide.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_abaloparatide.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_romo.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_romo.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_romo.male") %>% 
    select(Value)), 
    step=0.1)
})

output$monitored.12m_adh_alendronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_alendronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_alendronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_risedronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_risedronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_risedronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_strontium.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_strontium.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_strontium.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_ibandronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_ibandronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_ibandronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_raloxifene.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_raloxifene.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_raloxifene.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_denosumab.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_denosumab.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_denosumab.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_zoledronate.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_zoledronate.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_zoledronate.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_teriparatide.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_teriparatide.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_teriparatide.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_abaloparatide.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_abaloparatide.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_abaloparatide.male") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_romo.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_romo.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_romo.male") %>% 
    select(Value)), 
    step=0.1)
})


output$monitored.4m_adh_alendronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_alendronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_alendronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_risedronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_risedronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_risedronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_strontium.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_strontium.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_strontium.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_ibandronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_ibandronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_ibandronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_raloxifene.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_raloxifene.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_raloxifene.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_denosumab.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_denosumab.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_denosumab.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_zoledronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_zoledronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_zoledronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_teriparatide.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_teriparatide.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_teriparatide.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_abaloparatide.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_abaloparatide.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_abaloparatide.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.4m_adh_romo.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.4m_adh_romo.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.4m_adh_romo.female") %>% 
    select(Value)), 
    step=0.1)
})

output$monitored.12m_adh_alendronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_alendronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_alendronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_risedronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_risedronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_risedronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_strontium.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_strontium.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_strontium.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_ibandronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_ibandronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_ibandronate.female") %>% 
    select(Value)), 
    step=0.1)
})

output$monitored.12m_adh_raloxifene.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_raloxifene.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_raloxifene.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_denosumab.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_denosumab.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_denosumab.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_zoledronate.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_zoledronate.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_zoledronate.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_teriparatide.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_teriparatide.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_teriparatide.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_abaloparatide.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_abaloparatide.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_abaloparatide.female") %>% 
    select(Value)), 
    step=0.1)
})
output$monitored.12m_adh_romo.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('monitored.12m_adh_romo.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="monitored.12m_adh_romo.female") %>% 
    select(Value)), 
    step=0.1)
})





# adherence after year 2 -----
output$adh_annual_decline_alendronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_alendronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_alendronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_risedronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_risedronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_risedronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_strontium.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_strontium.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_strontium.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_ibandronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_ibandronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_ibandronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_raloxifene.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_raloxifene.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_raloxifene.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_denosumab.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_denosumab.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_denosumab.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_zoledronate.no.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_zoledronate.no.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_zoledronate.no.fls.male") %>% 
    select(Value)), 
    step=0.1)
})

output$adh_annual_decline_alendronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_alendronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_alendronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_risedronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_risedronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_risedronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_strontium.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_strontium.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_strontium.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_ibandronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_ibandronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_ibandronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_raloxifene.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_raloxifene.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_raloxifene.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_denosumab.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_denosumab.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_denosumab.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_zoledronate.no.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_zoledronate.no.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_zoledronate.no.fls.female") %>% 
    select(Value)), 
    step=0.1)
})

output$adh_annual_decline_alendronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_alendronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_alendronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_risedronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_risedronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_risedronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_strontium.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_strontium.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_strontium.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_ibandronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_ibandronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_ibandronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_raloxifene.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_raloxifene.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_raloxifene.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_denosumab.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_denosumab.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_denosumab.fls.male") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_zoledronate.fls.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_zoledronate.fls.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_zoledronate.fls.male") %>% 
    select(Value)), 
    step=0.1)
})

output$adh_annual_decline_alendronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_alendronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_alendronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_risedronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_risedronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_risedronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_strontium.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_strontium.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_strontium.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_ibandronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_ibandronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_ibandronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_raloxifene.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_raloxifene.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_raloxifene.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_denosumab.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_denosumab.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_denosumab.fls.female") %>% 
    select(Value)), 
    step=0.1)
})
output$adh_annual_decline_zoledronate.fls.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('adh_annual_decline_zoledronate.fls.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="adh_annual_decline_zoledronate.fls.female") %>% 
    select(Value)), 
    step=0.1)
})


# mortality -----
output$prob.death_hf1_0_3.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf1_0_3.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf1_0_3.male") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_hf1_0_12.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf1_0_12.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf1_0_12.male") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_hf1_0_60.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf1_0_60.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf1_0_60.male") %>% 
    select(Value)), 
    step=0.1)
})

output$prob.death_hf2_0_3.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf2_0_3.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf2_0_3.male") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_hf2_0_12.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf2_0_12.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf2_0_12.male") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_hf2_0_60.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf2_0_60.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf2_0_60.male") %>% 
    select(Value)), 
    step=0.1)
})

output$prob.death_sf_0_3.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_sf_0_3.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_sf_0_3.male") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_sf_0_12.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_sf_0_12.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_sf_0_12.male") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_sf_0_60.male <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_sf_0_60.male', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_sf_0_60.male") %>% 
    select(Value)), 
    step=0.1)
})

output$prob.death_hf1_0_3.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf1_0_3.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf1_0_3.female") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_hf1_0_12.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf1_0_12.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf1_0_12.female") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_hf1_0_60.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf1_0_60.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf1_0_60.female") %>% 
    select(Value)), 
    step=0.1)
})

output$prob.death_hf2_0_3.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf2_0_3.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf2_0_3.female") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_hf2_0_12.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf2_0_12.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf2_0_12.female") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_hf2_0_60.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_hf2_0_60.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_hf2_0_60.female") %>% 
    select(Value)), 
    step=0.1)
})

output$prob.death_sf_0_3.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_sf_0_3.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_sf_0_3.female") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_sf_0_12.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_sf_0_12.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_sf_0_12.female") %>% 
    select(Value)), 
    step=0.1)
})
output$prob.death_sf_0_60.female <- renderUI({
  working.country_data<-get.working.country_data()
  
   numericInput('prob.death_sf_0_60.female', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prob.death_sf_0_60.female") %>% 
    select(Value)), 
    step=0.1)
})


# QOL -----
output$qol.pre.hip <- renderUI({
   numericInput('qol.pre.hip', '',
                value=as.numeric(get.working.country_qol.data() %>%
    filter(name=="qol.pre.hip") %>%
    select(Value)),
    step=0.1)
})

output$qol.post.hip <- renderUI({
   numericInput('qol.post.hip', '',
                value=as.numeric(get.working.country_qol.data() %>%
    filter(name=="qol.post.hip") %>%
    select(Value)),
    step=0.1)
})

output$qol.post.4.month.hip <- renderUI({
   numericInput('qol.post.4.month.hip', '',
                value=as.numeric(get.working.country_qol.data() %>%
    filter(name=="qol.post.4.month.hip") %>%
    select(Value)),
    step=0.1)
})



output$qol.pre.spine <- renderUI({
   numericInput('qol.pre.spine', '',
                value=as.numeric(get.working.country_qol.data() %>%
    filter(name=="qol.pre.spine") %>%
    select(Value)),
    step=0.1)
})

output$qol.post.spine <- renderUI({
   numericInput('qol.post.spine', '',
                value=as.numeric(get.working.country_qol.data() %>%
    filter(name=="qol.post.spine") %>%
    select(Value)),
    step=0.1)
})

output$qol.post.4.month.spine <- renderUI({
   numericInput('qol.post.4.month.spine', '',
                value=as.numeric(get.working.country_qol.data() %>%
    filter(name=="qol.post.4.month.spine") %>%
    select(Value)),
    step=0.1)
})



output$qol.pre.other <- renderUI({
   numericInput('qol.pre.other', '',
                value=as.numeric(get.working.country_qol.data() %>%
    filter(name=="qol.pre.other") %>%
    select(Value)),
    step=0.1)
})

output$qol.post.other <- renderUI({
   numericInput('qol.post.other', '',
                value=as.numeric(get.working.country_qol.data() %>%
    filter(name=="qol.post.other") %>%
    select(Value)),
    step=0.1)
})

output$qol.post.4.month.other <- renderUI({
   numericInput('qol.post.4.month.other', '',
                value=as.numeric(get.working.country_qol.data() %>%
    filter(name=="qol.post.4.month.other") %>%
    select(Value)),
    step=0.1)
})



## COSTING ------
#Treatment following fracture ----
output$prop.admitted.surgery.hip <- renderUI({
   numericInput('prop.admitted.surgery.hip', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.admitted.surgery.hip") %>%
    select(Value)),
    step=0.1)
})
output$prop.admitted.no.surgery.hip <- renderUI({
   numericInput('prop.admitted.no.surgery.hip', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.admitted.no.surgery.hip") %>%
    select(Value)),
    step=0.1)
})
output$prop.not.admitted.clinic.hip <- renderUI({
   numericInput('prop.not.admitted.clinic.hip', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.not.admitted.clinic.hip") %>%
    select(Value)),
    step=0.1)
})


output$prop.hospital.spine <- renderUI({
   numericInput('prop.hospital.spine', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.hospital.spine") %>%
    select(Value)),
    step=0.1)
})
output$prop.hospital.community.spine <- renderUI({
   numericInput('prop.hospital.community.spine', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.hospital.community.spine") %>%
    select(Value)),
    step=0.1)
})

output$prop.hospital.spine.kyphoplasty <- renderUI({
   numericInput('prop.hospital.spine.kyphoplasty', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.hospital.spine.kyphoplasty") %>%
    select(Value)),
    step=0.1)
})
output$prop.hospital.spine.vertebroplasty <- renderUI({
   numericInput('prop.hospital.spine.vertebroplasty', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.hospital.spine.vertebroplasty") %>%
    select(Value)),
    step=0.1)
})
output$prop.hospital.spine.no.intervention <- renderUI({
   numericInput('prop.hospital.spine.no.intervention', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.hospital.spine.no.intervention") %>%
    select(Value)),
    step=0.1)
})
output$prop.not.admitted.spine.kyphoplasty <- renderUI({
   numericInput('prop.not.admitted.spine.kyphoplasty', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.not.admitted.spine.kyphoplasty") %>%
    select(Value)),
    step=0.1)
})
output$prop.not.admitted.spine.vertebroplasty <- renderUI({
   numericInput('prop.not.admitted.spine.vertebroplasty', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.not.admitted.spine.vertebroplasty") %>%
    select(Value)),
    step=0.1)
})
output$prop.not.admitted.spine.no.intervention <- renderUI({
   numericInput('prop.not.admitted.spine.no.intervention', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.not.admitted.spine.no.intervention") %>%
    select(Value)),
    step=0.1)
})

output$prop.admitted.surgery.other <- renderUI({
   numericInput('prop.admitted.surgery.other', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.admitted.surgery.other") %>%
    select(Value)),
    step=0.1)
})
output$prop.admitted.no.surgery.other <- renderUI({
   numericInput('prop.admitted.no.surgery.other', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.admitted.no.surgery.other") %>%
    select(Value)),
    step=0.1)
})
output$prop.not.admitted.clinic.other <- renderUI({
   numericInput('prop.not.admitted.clinic.other', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="prop.not.admitted.clinic.other") %>%
    select(Value)),
    step=0.1)
})
# hosp los-----
output$hospital.los.hip.surg <- renderUI({
   numericInput('hospital.los.hip.surg', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hospital.los.hip.surg") %>%
    select(Value)),
    step=0.1)
})
output$hospital.los.hip.no.surg <- renderUI({
   numericInput('hospital.los.hip.no.surg', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hospital.los.hip.no.surg") %>%
    select(Value)),
    step=0.1)
})
output$hospital.los.spine.kyphoplasty <- renderUI({
   numericInput('hospital.los.spine.kyphoplasty', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hospital.los.spine.kyphoplasty") %>%
    select(Value)),
    step=0.1)
})
output$hospital.los.spine.vertebroplasty <- renderUI({
   numericInput('hospital.los.spine.vertebroplasty', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hospital.los.spine.vertebroplasty") %>%
    select(Value)),
    step=0.1)
})
output$hospital.los.spine.no.surg <- renderUI({
   numericInput('hospital.los.spine.no.surg', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hospital.los.spine.no.surg") %>%
    select(Value)),
    step=0.1)
})

output$hospital.los.other.surg <- renderUI({
   numericInput('hospital.los.other.surg', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hospital.los.other.surg") %>%
    select(Value)),
    step=0.1)
})

output$hospital.los.other.no.surg <- renderUI({
   numericInput('hospital.los.other.no.surg', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hospital.los.other.no.surg") %>%
    select(Value)),
    step=0.1)
})

output$cost.a_e.visit <- renderUI({
   numericInput('cost.a_e.visit', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="cost.a_e.visit") %>%
    select(Value)),
    step=0.1)
})
output$cost.hosp.per.day <- renderUI({
   numericInput('cost.hosp.per.day', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="cost.hosp.per.day") %>%
    select(Value)),
    step=0.1)
})
output$cost.hosp.clinic.visit <- renderUI({
   numericInput('cost.hosp.clinic.visit', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="cost.hosp.clinic.visit") %>%
    select(Value)),
    step=0.1)
})


output$cost.hip.surg <- renderUI({
   numericInput('cost.hip.surg', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="cost.hip.surg") %>%
    select(Value)),
    step=0.1)
})
output$cost.spine.kyphoplasty <- renderUI({
   numericInput('cost.spine.kyphoplasty', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="cost.spine.kyphoplasty") %>%
    select(Value)),
    step=0.1)
})
output$cost.spine.vertebroplasty <- renderUI({
   numericInput('cost.spine.vertebroplasty', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="cost.spine.vertebroplasty") %>%
    select(Value)),
    step=0.1)
})
output$cost.other.surg <- renderUI({
   numericInput('cost.other.surg', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="cost.other.surg") %>%
    select(Value)),
    step=0.1)
})



output$visits.comm.consults.spine <- renderUI({
   numericInput('visits.comm.consults.spine', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="visits.comm.consults.spine") %>%
    select(Value)),
    step=0.1)
})
output$cost.spine.community.care <- renderUI({
   numericInput('cost.spine.community.care', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="cost.spine.community.care") %>%
    select(Value)),
    step=0.1)
})

# fracture prevention ----
#### no.fls hip
output$no.fls.administrator.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.administrator.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.administrator.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.administrator.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.nurse.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.nurse.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.nurse.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.nurse.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.doctor.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.doctor.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.doctor.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.doctor.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.radiographer.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.radiographer.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.radiographer.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.radiographer.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.allied_health.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.allied_health.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.allied_health.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.allied_health.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.other.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.other.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.other.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.other.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

#### no.fls spine
output$no.fls.administrator.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.administrator.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.administrator.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.administrator.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.nurse.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.nurse.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.nurse.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.nurse.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.doctor.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.doctor.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.doctor.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.doctor.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.radiographer.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.radiographer.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.radiographer.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.radiographer.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.allied_health.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.allied_health.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.allied_health.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.allied_health.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.other.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.other.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.other.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.other.monitoring.mins.spine <- renderUI({

  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

#### no.fls other
output$no.fls.administrator.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.administrator.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.administrator.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.administrator.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.administrator.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.administrator.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.nurse.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.nurse.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.nurse.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.nurse.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.nurse.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.nurse.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.doctor.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.doctor.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.doctor.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.doctor.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.doctor.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.doctor.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.radiographer.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.radiographer.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.radiographer.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.radiographer.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.radiographer.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.radiographer.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.allied_health.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.allied_health.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.allied_health.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.allied_health.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.allied_health.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.allied_health.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$no.fls.other.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.other.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.other.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$no.fls.other.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('no.fls.other.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="no.fls.other.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})







#### fls hip
output$fls.administrator.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.administrator.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.administrator.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.administrator.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.fls_coordinator.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.fls_coordinator.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.fls_coordinator.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.fls_coordinator.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})


output$fls.nurse.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.nurse.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.nurse.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.nurse.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.doctor.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.doctor.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.doctor.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.doctor.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.radiographer.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.radiographer.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.radiographer.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.radiographer.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.allied_health.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.allied_health.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.allied_health.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.allied_health.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.other.identification.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.identification.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.identification.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.other.assessment.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.assessment.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.assessment.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.other.recommendation.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.recommendation.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.recommendation.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.other.monitoring.mins.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.monitoring.mins.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.monitoring.mins.hip") %>% 
    select(Value)), 
    step=0.1)
})

#### fls spine
output$fls.administrator.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.administrator.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.administrator.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.administrator.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.fls_coordinator.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.fls_coordinator.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.fls_coordinator.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.fls_coordinator.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})


output$fls.nurse.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.nurse.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.nurse.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.nurse.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.doctor.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.doctor.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.doctor.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.doctor.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.radiographer.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.radiographer.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.radiographer.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.radiographer.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.allied_health.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.allied_health.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.allied_health.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.allied_health.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.other.identification.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.identification.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.identification.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.other.assessment.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.assessment.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.assessment.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.other.recommendation.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.recommendation.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.recommendation.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.other.monitoring.mins.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.monitoring.mins.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.monitoring.mins.spine") %>% 
    select(Value)), 
    step=0.1)
})


#### fls other
output$fls.administrator.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.administrator.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.administrator.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.administrator.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.administrator.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.administrator.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.fls_coordinator.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.fls_coordinator.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.fls_coordinator.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.fls_coordinator.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.fls_coordinator.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.fls_coordinator.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})


output$fls.nurse.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.nurse.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.nurse.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.nurse.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.nurse.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.nurse.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.doctor.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.doctor.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.doctor.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.doctor.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.doctor.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.doctor.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.radiographer.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.radiographer.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.radiographer.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.radiographer.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.radiographer.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.radiographer.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.allied_health.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.allied_health.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.allied_health.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.allied_health.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.allied_health.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.allied_health.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})

output$fls.other.identification.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.identification.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.identification.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.other.assessment.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.assessment.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.assessment.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.other.recommendation.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.recommendation.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.recommendation.mins.other") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.other.monitoring.mins.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.other.monitoring.mins.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.other.monitoring.mins.other") %>% 
    select(Value)), 
    step=0.1)
})


output$prop.lab.test.no.fls.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.lab.test.no.fls.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.lab.test.no.fls.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.lab.test.no.fls.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.lab.test.no.fls.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.lab.test.no.fls.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.lab.test.no.fls.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.lab.test.no.fls.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.lab.test.no.fls.other") %>% 
    select(Value)), 
    step=0.1)
})
 

       
output$prop.lab.test.fls.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.lab.test.fls.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.lab.test.fls.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.lab.test.fls.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.lab.test.fls.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.lab.test.fls.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.lab.test.fls.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.lab.test.fls.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.lab.test.fls.other") %>% 
    select(Value)), 
    step=0.1)
})


output$cost.lab.test.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('cost.lab.test.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="cost.lab.test.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$cost.lab.test.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('cost.lab.test.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="cost.lab.test.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$cost.lab.test.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('cost.lab.test.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="cost.lab.test.other") %>% 
    select(Value)), 
    step=0.1)
})




output$prop.dxa.no.fls.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.dxa.no.fls.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.dxa.no.fls.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.dxa.no.fls.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.dxa.no.fls.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.dxa.no.fls.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.dxa.no.fls.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.dxa.no.fls.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.dxa.no.fls.other") %>% 
    select(Value)), 
    step=0.1)
})

output$prop.dxa.fls.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.dxa.fls.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.dxa.fls.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.dxa.fls.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.dxa.fls.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.dxa.fls.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.dxa.fls.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.dxa.fls.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.dxa.fls.other") %>% 
    select(Value)), 
    step=0.1)
})

output$cost.dxa <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('cost.dxa', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="cost.dxa") %>% 
    select(Value)), 
    step=0.1)
})



output$Alendronate.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Alendronate.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Alendronate.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$Risedronate.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Risedronate.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Risedronate.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$Ibandronate.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Ibandronate.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Ibandronate.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$Raloxifene.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Raloxifene.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Raloxifene.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$Strontium.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Strontium.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Strontium.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$Denosumab.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Denosumab.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Denosumab.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$Zoledronate.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Zoledronate.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Zoledronate.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$Teriparatide.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Teriparatide.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Teriparatide.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$Abaloparatide.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Abaloparatide.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Abaloparatide.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$Romo.yearly.cost.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Romo.yearly.cost.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Romo.yearly.cost.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$Alendronate.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Alendronate.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Alendronate.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$Risedronate.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Risedronate.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Risedronate.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$Ibandronate.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Ibandronate.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Ibandronate.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$Raloxifene.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Raloxifene.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Raloxifene.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$Strontium.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Strontium.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Strontium.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$Denosumab.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Denosumab.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Denosumab.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$Zoledronate.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Zoledronate.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Zoledronate.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$Teriparatide.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Teriparatide.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Teriparatide.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$Abaloparatide.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Abaloparatide.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Abaloparatide.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$Romo.yearly.cost.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Romo.yearly.cost.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Romo.yearly.cost.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$Alendronate.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Alendronate.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Alendronate.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})
output$Risedronate.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Risedronate.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Risedronate.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})
output$Ibandronate.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Ibandronate.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Ibandronate.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})
output$Raloxifene.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Raloxifene.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Raloxifene.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})
output$Strontium.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Strontium.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Strontium.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})
output$Denosumab.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Denosumab.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Denosumab.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})
output$Zoledronate.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Zoledronate.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Zoledronate.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})
output$Teriparatide.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Teriparatide.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Teriparatide.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})
output$Abaloparatide.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Abaloparatide.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Abaloparatide.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})
output$Romo.yearly.cost.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('Romo.yearly.cost.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="Romo.yearly.cost.other") %>% 
    select(Value)), 
    step=0.1)
})



output$fls.database.monthly.maintenance.cost <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.database.monthly.maintenance.cost', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.database.monthly.maintenance.cost") %>% 
    select(Value)), 
    step=0.1)
})
output$fls.database.installation.cost <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('fls.database.installation.cost', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="fls.database.installation.cost") %>% 
    select(Value)), 
    step=0.1)
})
output$software.monthly.maintenance.cost <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('software.monthly.maintenance.cost', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="software.monthly.maintenance.cost") %>% 
    select(Value)), 
    step=0.1)
})
output$software.installation.cost <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('software.installation.cost', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="software.installation.cost") %>% 
    select(Value)), 
    step=0.1)
})





output$prop.discharged.temp.rehab.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.temp.rehab.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.temp.rehab.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.home.no.support.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.home.no.support.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.home.no.support.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.home.support.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.home.support.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.home.support.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.family.home.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.family.home.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.family.home.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.long.term.care.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.long.term.care.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.long.term.care.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$prop.discharged.temp.rehab.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.temp.rehab.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.temp.rehab.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.home.no.support.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.home.no.support.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.home.no.support.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.home.support.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.home.support.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.home.support.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.family.home.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.family.home.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.family.home.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.long.term.care.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.long.term.care.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.long.term.care.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$prop.discharged.temp.rehab.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.temp.rehab.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.temp.rehab.other") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.home.no.support.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.home.no.support.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.home.no.support.other") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.home.support.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.home.support.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.home.support.other") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.family.home.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.family.home.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.family.home.other") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.discharged.long.term.care.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.discharged.long.term.care.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.discharged.long.term.care.other") %>% 
    select(Value)), 
    step=0.1)
})


output$temp.rehab.los.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('temp.rehab.los.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="temp.rehab.los.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$temp.rehab.los.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('temp.rehab.los.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="temp.rehab.los.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$temp.rehab.los.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('temp.rehab.los.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="temp.rehab.los.other") %>% 
    select(Value)), 
    step=0.1)
})


output$prop.rehab.to.home.no.support.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.home.no.support.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.home.no.support.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.rehab.to.home.support.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.home.support.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.home.support.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.rehab.to.family.home.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.family.home.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.family.home.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.rehab.to.long.term.care.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.long.term.care.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.long.term.care.hip") %>% 
    select(Value)), 
    step=0.1)
})

output$prop.rehab.to.home.no.support.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.home.no.support.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.home.no.support.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.rehab.to.home.support.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.home.support.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.home.support.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.rehab.to.family.home.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.family.home.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.family.home.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.rehab.to.long.term.care.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.long.term.care.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.long.term.care.spine") %>% 
    select(Value)), 
    step=0.1)
})

output$prop.rehab.to.home.no.support.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.home.no.support.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.home.no.support.other") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.rehab.to.home.support.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.home.support.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.home.support.other") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.rehab.to.family.home.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.family.home.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.family.home.other") %>% 
    select(Value)), 
    step=0.1)
})
output$prop.rehab.to.long.term.care.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('prop.rehab.to.long.term.care.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="prop.rehab.to.long.term.care.other") %>% 
    select(Value)), 
    step=0.1)
})


output$clinic.visits.admitted.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('clinic.visits.admitted.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="clinic.visits.admitted.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$clinic.visits.admitted.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('clinic.visits.admitted.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="clinic.visits.admitted.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$clinic.visits.admitted.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('clinic.visits.admitted.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="clinic.visits.admitted.other") %>% 
    select(Value)), 
    step=0.1)
})

output$clinic.visits.not.admitted.spine <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('clinic.visits.not.admitted.spine', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="clinic.visits.not.admitted.spine") %>% 
    select(Value)), 
    step=0.1)
})
output$clinic.visits.not.admitted.hip <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('clinic.visits.not.admitted.hip', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="clinic.visits.not.admitted.hip") %>% 
    select(Value)), 
    step=0.1)
})
output$clinic.visits.not.admitted.other <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('clinic.visits.not.admitted.other', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="clinic.visits.not.admitted.other") %>% 
    select(Value)), 
    step=0.1)
})




output$temp.rehab.daily.cost <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('temp.rehab.daily.cost', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="temp.rehab.daily.cost") %>% 
    select(Value)), 
    step=0.1)
})
output$long.term.care.monthly.cost <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('long.term.care.monthly.cost', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="long.term.care.monthly.cost") %>% 
    select(Value)), 
    step=0.1)
})
output$care.home.monthly.cost <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('care.home.monthly.cost', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="care.home.monthly.cost") %>% 
    select(Value)), 
    step=0.1)
})
output$clinic.visit.cost <- renderUI({
  working.country_data<-get.working.country_cost.data()
  
   numericInput('clinic.visit.cost', '', 
                value=as.numeric(working.country_data %>% 
    filter(name=="clinic.visit.cost") %>% 
    select(Value)), 
    step=0.1)
})


# staff costs -----
output$hourly.cost.administrator <- renderUI({
   numericInput('hourly.cost.administrator', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hourly.cost.administrator") %>%
    select(Value)),
    step=0.1)
})

output$hourly.cost.nurse <- renderUI({
   numericInput('hourly.cost.nurse', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hourly.cost.nurse") %>%
    select(Value)),
    step=0.1)
})

output$hourly.cost.doctor <- renderUI({
   numericInput('hourly.cost.doctor', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hourly.cost.doctor") %>%
    select(Value)),
    step=0.1)
})

output$hourly.cost.radiographer <- renderUI({
   numericInput('hourly.cost.radiographer', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hourly.cost.radiographer") %>%
    select(Value)),
    step=0.1)
})

output$hourly.cost.allied_health <- renderUI({
   numericInput('hourly.cost.allied_health', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hourly.cost.allied_health") %>%
    select(Value)),
    step=0.1)
})

output$hourly.cost.other <- renderUI({
   numericInput('hourly.cost.other', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hourly.cost.other") %>%
    select(Value)),
    step=0.1)
})

output$hourly.cost.fls_coordinator <- renderUI({
   numericInput('hourly.cost.fls_coordinator', '',
                value=as.numeric(get.working.country_cost.data() %>%
    filter(name=="hourly.cost.fls_coordinator") %>%
    select(Value)),
    step=0.1)
})


##### MODEL INPUTS ------
# which index fx was chosen?-----

get.chosen_index_fx_option<-reactive({ 
if(input$checkbox_index_fx_option_1==TRUE){
  chosen<-"all"}
 if(input$checkbox_index_fx_option_2==TRUE){
   chosen<-"hip only"}  
 if(input$checkbox_index_fx_option_3==TRUE){
   chosen<-"pop"}
  chosen
})

output$chosen_index_fx_option <- renderText({ 
  get.chosen_index_fx_option()
})

# study_pop_n -----
get.study_pop_n<-reactive({
 chosen<-get.chosen_index_fx_option()
 
 if(chosen=="all"){
study_pop_n<-data.frame(
  names=c("spine_fx_n.male",
  "hip_fx_n.male",
  "other_fx_n.male",
  "spine_fx_n.female",
  "hip_fx_n.female",
  "other_fx_n.female"),
n=c(input$all_spine_fx_n.male,
input$all_hip_fx_n.male,
input$all_other_fx_n.male,
input$all_spine_fx_n.female,
input$all_hip_fx_n.female,
input$all_other_fx_n.female))}
 
 
  if(chosen=="hip only"){
  study_pop_n<-data.frame(
  names=c("spine_fx_n.male",
  "hip_fx_n.male",
  "other_fx_n.male",
  "spine_fx_n.female",
  "hip_fx_n.female",
  "other_fx_n.female"),
n=c(round(input$only_hip_fx_n.male * 0.75),
input$only_hip_fx_n.male,
 round((5*input$only_hip_fx_n.male)-
  input$only_hip_fx_n.male-
  (input$only_hip_fx_n.male * 0.75)),
round(input$only_hip_fx_n.female * 0.75),
input$only_hip_fx_n.female,
 round((5*input$only_hip_fx_n.female)-
  input$only_hip_fx_n.female-
  (input$only_hip_fx_n.female * 0.75))))
  }
 
   if(chosen=="pop"){
     working.country_data<-get.working.country_data()
    # browser()
 hip_fx_n.male<-  round(
   input$pop_size* #general pop
      input$gen_pop_over_50*  # proportion over 50
       (1-as.numeric(working.country_data %>% filter(name=="over_50.female") %>% select(Value) %>% pull())) *   #proportion male
        input$prob_hip_fx_over_50.male # prob hip fx
 ) 
 
  hip_fx_n.female<-  round(
   input$pop_size* #general pop
      input$gen_pop_over_50*  # proportion over 50
       (as.numeric(working.country_data %>% filter(name=="over_50.female") %>% select(Value) %>% pull())) *   #proportion female
        input$prob_hip_fx_over_50.female # prob hip fx
 )
 
  
  
    study_pop_n<-data.frame(
  names=c("spine_fx_n.male",
  "hip_fx_n.male",
  "other_fx_n.male",
  "spine_fx_n.female",
  "hip_fx_n.female",
  "other_fx_n.female"),
n=c(round(hip_fx_n.male * 0.75),
hip_fx_n.male,
 round((5*hip_fx_n.male)-
  hip_fx_n.male-
  (hip_fx_n.male * 0.75)),
round(hip_fx_n.female * 0.75),
hip_fx_n.female,
 round((5*hip_fx_n.female)-
  hip_fx_n.female-
  (hip_fx_n.female * 0.75))))
  
 
     
   }
 

 study_pop_n
 
 
 
  
  })


lapply(c(
         "av_age_spine.male",
"av_age_hip.male",
"av_age_other.male",
"av_age_spine.female",
"av_age_hip.female",
"av_age_other.female"
         ),
       function(x) outputOptions(output, x, suspendWhenHidden = FALSE))

get.study_pop_n.age<-reactive({
  study_pop_n<-get.study_pop_n()
 
  cbind(study_pop_n, 
        data.frame(age=c(
          input$av_age_spine.male,
          input$av_age_hip.male,
          input$av_age_other.male,
          input$av_age_spine.female,
          input$av_age_hip.female,
          input$av_age_other.female)))
  
  
})

output$study_pop_n<-renderTable({
  get.study_pop_n.age()
  
})

# microsimulation population ----

get.microsim_pop<-reactive({
  
 study_pop_n<- get.study_pop_n.age()
  
microsim_pop<-NULL
# 
if(as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.male") %>% 
  select(n))>0){
  microsim_pop<-rbind(microsim_pop,
  data.frame(
  index_fx=c(rep("spine", n_microsimulation)),
  sex=c(rep("male",n_microsimulation)),
  age=c(rep(input$av_age_spine.male,n_microsimulation))))
}
if(as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.male") %>% 
  select(n))>0){
  microsim_pop<-rbind(microsim_pop,
  data.frame(
  index_fx=c(rep("hip", n_microsimulation)),
  sex=c(rep("male",n_microsimulation)),
  age=c(rep(input$av_age_hip.male,n_microsimulation))))
}
if(as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.male") %>% 
  select(n))>0){
  microsim_pop<-rbind(microsim_pop,
  data.frame(
  index_fx=c(rep("other", n_microsimulation)),
  sex=c(rep("male",n_microsimulation)),
  age=c(rep(input$av_age_other.male,n_microsimulation))))
}
if(as.numeric(study_pop_n %>% 
  filter(names=="spine_fx_n.female") %>% 
  select(n))>0){
  microsim_pop<-rbind(microsim_pop,
  data.frame(
  index_fx=c(rep("spine", n_microsimulation)),
  sex=c(rep("female",n_microsimulation)),
  age=c(rep(input$av_age_spine.female,n_microsimulation))))
}
if(as.numeric(study_pop_n %>% 
  filter(names=="hip_fx_n.female") %>% 
  select(n))>0){
  microsim_pop<-rbind(microsim_pop,
  data.frame(
  index_fx=c(rep("hip", n_microsimulation)),
  sex=c(rep("female",n_microsimulation)),
  age=c(rep(input$av_age_hip.female,n_microsimulation))))
}
if(as.numeric(study_pop_n %>% 
  filter(names=="other_fx_n.female") %>% 
  select(n))>0){
  microsim_pop<-rbind(microsim_pop,
  data.frame(
  index_fx=c(rep("other", n_microsimulation)),
  sex=c(rep("female",n_microsimulation)),
  age=c(rep(input$av_age_other.female,n_microsimulation))))
}

microsim_pop$id<-seq(1:nrow(microsim_pop))

 
 
 # microsim_pop<-data.frame(id=1:(n_microsimulation*6),
 #  index_fx=c(rep("spine", n_microsimulation*2),
 #             rep("hip", n_microsimulation*2),
 #             rep("other", n_microsimulation*2)),
 #  sex=c(rep("male",n_microsimulation),
 #      rep("female",n_microsimulation),
 #      rep("male",n_microsimulation),
 #      rep("female",n_microsimulation),
 #      rep("male",n_microsimulation),
 #      rep("female",n_microsimulation)),
 #  age=c(rep(input$av_age_spine.male,n_microsimulation),
 #        rep(input$av_age_spine.female,n_microsimulation),
 #        rep(input$av_age_hip.male,n_microsimulation),
 #        rep(input$av_age_hip.female,n_microsimulation),
 #        rep(input$av_age_other.male,n_microsimulation),
 #        rep(input$av_age_other.female,n_microsimulation)))
  
# add age from study pop
 # study_pop_n.age %>% 
 #   filter
 microsim_pop
  
  })

output$microsim_pop<-renderTable({
  
  get.microsim_pop()
})

# view unseen inputs ----
lapply(c(
         "risk_hip_fracture_after_hip_5y.male",
"risk_spine_fracture_after_hip_5y.male",
"risk_other_fracture_after_hip_5y.male",
         "risk_hip_fracture_after_spine_5y.male",
"risk_spine_fracture_after_spine_5y.male",
"risk_other_fracture_after_spine_5y.male",
         "risk_hip_fracture_after_other_5y.male",
"risk_spine_fracture_after_other_5y.male",
"risk_other_fracture_after_other_5y.male",
         "risk_hip_fracture_after_hip_5y.female",
"risk_spine_fracture_after_hip_5y.female",
"risk_other_fracture_after_hip_5y.female",
         "risk_hip_fracture_after_spine_5y.female",
"risk_spine_fracture_after_spine_5y.female",
"risk_other_fracture_after_spine_5y.female",
         "risk_hip_fracture_after_other_5y.female",
"risk_spine_fracture_after_other_5y.female",
"risk_other_fracture_after_other_5y.female"  , 
"prob.death_hf1_0_3.male",
"prob.death_hf1_0_12.male",
"prob.death_hf1_0_60.male",
"prob.death_hf2_0_3.male",
"prob.death_hf2_0_12.male",
"prob.death_hf2_0_60.male",
"prob.death_sf_0_3.male",
"prob.death_sf_0_12.male",
"prob.death_sf_0_60.male",
"prob.death_hf1_0_3.female",
"prob.death_hf1_0_12.female",
"prob.death_hf1_0_60.female",
"prob.death_hf2_0_3.female",
"prob.death_hf2_0_12.female",
"prob.death_hf2_0_60.female",
"prob.death_sf_0_3.female",
"prob.death_sf_0_12.female",
"prob.death_sf_0_60.female"


),
       function(x) outputOptions(output, x, suspendWhenHidden = FALSE))







# tps background  ------

get.background.tps<-reactive({
## background risk of fracture -----
# if 5y
if(input$checkbox_index_refx_5y==TRUE){
background.tps<-  data.frame(
  time=timing_fx_risk.5y$Month,
tp.hf_hf.male= timing_fx_risk.5y$Hip*
  input$risk_hip_fracture_after_hip_5y.male,
tp.hf_sf.male= timing_fx_risk.5y$Hip*
  input$risk_spine_fracture_after_hip_5y.male,
tp.hf_of.male= timing_fx_risk.5y$Hip*
  input$risk_other_fracture_after_hip_5y.male,
tp.sf_hf.male= timing_fx_risk.5y$Spine*
  input$risk_hip_fracture_after_spine_5y.male,
tp.sf_sf.male= timing_fx_risk.5y$Spine*
  input$risk_spine_fracture_after_spine_5y.male,
tp.sf_of.male= timing_fx_risk.5y$Spine*
  input$risk_other_fracture_after_spine_5y.male,
tp.of_hf.male= timing_fx_risk.5y$Other*
  input$risk_hip_fracture_after_other_5y.male,
tp.of_sf.male= timing_fx_risk.5y$Other*
  input$risk_spine_fracture_after_other_5y.male,
tp.of_of.male= timing_fx_risk.5y$Other*
  input$risk_other_fracture_after_other_5y.male,

tp.hf_hf.female= timing_fx_risk.5y$Hip*
  input$risk_hip_fracture_after_hip_5y.female,
tp.hf_sf.female= timing_fx_risk.5y$Hip*
  input$risk_spine_fracture_after_hip_5y.female,
tp.hf_of.female= timing_fx_risk.5y$Hip*
  input$risk_other_fracture_after_hip_5y.female,
tp.sf_hf.female= timing_fx_risk.5y$Spine*
  input$risk_hip_fracture_after_spine_5y.female,
tp.sf_sf.female= timing_fx_risk.5y$Spine*
  input$risk_spine_fracture_after_spine_5y.female,
tp.sf_of.female= timing_fx_risk.5y$Spine*
  input$risk_other_fracture_after_spine_5y.female,
tp.of_hf.female= timing_fx_risk.5y$Other*
  input$risk_hip_fracture_after_other_5y.female,
tp.of_sf.female= timing_fx_risk.5y$Other*
  input$risk_spine_fracture_after_other_5y.female,
tp.of_of.female= timing_fx_risk.5y$Other*
  input$risk_other_fracture_after_other_5y.female)
    }
# if 10y
if(input$checkbox_index_refx_10y==TRUE){
background.tps<-  data.frame(
  time=timing_fx_risk.10y$Month,
tp.hf_hf.male= timing_fx_risk.10y$Hip*
  input$risk_hip_fracture_after_hip_10y.male,
tp.hf_sf.male= timing_fx_risk.10y$Hip*
  input$risk_spine_fracture_after_hip_10y.male,
tp.hf_of.male= timing_fx_risk.10y$Hip*
  input$risk_other_fracture_after_hip_10y.male,
tp.sf_hf.male= timing_fx_risk.10y$Spine*
  input$risk_hip_fracture_after_spine_10y.male,
tp.sf_sf.male= timing_fx_risk.10y$Spine*
  input$risk_spine_fracture_after_spine_10y.male,
tp.sf_of.male= timing_fx_risk.10y$Spine*
  input$risk_other_fracture_after_spine_10y.male,
tp.of_hf.male= timing_fx_risk.10y$Other*
  input$risk_hip_fracture_after_other_10y.male,
tp.of_sf.male= timing_fx_risk.10y$Other*
  input$risk_spine_fracture_after_other_10y.male,
tp.of_of.male= timing_fx_risk.10y$Other*
  input$risk_other_fracture_after_other_10y.male,

tp.hf_hf.female= timing_fx_risk.10y$Hip*
  input$risk_hip_fracture_after_hip_10y.female,
tp.hf_sf.female= timing_fx_risk.10y$Hip*
  input$risk_spine_fracture_after_hip_10y.female,
tp.hf_of.female= timing_fx_risk.10y$Hip*
  input$risk_other_fracture_after_hip_10y.female,
tp.sf_hf.female= timing_fx_risk.10y$Spine*
  input$risk_hip_fracture_after_spine_10y.female,
tp.sf_sf.female= timing_fx_risk.10y$Spine*
  input$risk_spine_fracture_after_spine_10y.female,
tp.sf_of.female= timing_fx_risk.10y$Spine*
  input$risk_other_fracture_after_spine_10y.female,
tp.of_hf.female= timing_fx_risk.10y$Other*
  input$risk_hip_fracture_after_other_10y.female,
tp.of_sf.female= timing_fx_risk.10y$Other*
  input$risk_spine_fracture_after_other_10y.female,
tp.of_of.female= timing_fx_risk.10y$Other*
  input$risk_other_fracture_after_other_10y.female)
    }
  #tp.fx

    
## background risk of mortality ----

    if(input$country.name=="Colombia"){
   working.lifetable<-colombia.mortality 
    }
    if(input$country.name=="France"){
   working.lifetable<-france.mortality 
    }
    if(input$country.name=="Mexico"){
   working.lifetable<-mexico.mortality 
    }
    if(input$country.name=="Brazil"){
   working.lifetable<-brazil.mortality 
 }
  
  if(input$country.name=="Spain"){
   working.lifetable<-spain.mortality 
 }
 if(input$country.name %in% c("Japan", "Japan (reduced PFC)","Japan (inject only)")){
    working.lifetable<-japan.mortality }
 if(input$country.name=="United Kingdom"){
   working.lifetable<-uk.mortality }
 if(input$country.name %in% c("Netherlands", "Netherlands (perfect PFC)")){ 
    working.lifetable<-netherlands.mortality }

   if(input$country.name=="Generic country"){
   working.lifetable<-uk.mortality }
    
  
  
# male OF (background mortality) -----
of_tp.male<-working.lifetable %>% 
     filter(age %in% seq(input$av_age_other.male, input$av_age_other.male+4))
# to monthly 
of_tp.male<-data.frame(
  t=c(0,12,24, 36,48, 60),
  tp=c(NA, of_tp.male$male))

of_tp.male<-data.frame(time=1:60, 
  tp=c(rep(tp.different.cycle.length(of_tp.male$tp[2],12),12),
       rep(tp.different.cycle.length(of_tp.male$tp[3],12),12),
       rep(tp.different.cycle.length(of_tp.male$tp[4],12),12),
       rep(tp.different.cycle.length(of_tp.male$tp[5],12),12),
       rep(tp.different.cycle.length(of_tp.male$tp[6],12),12)))
  


background.tps$tp.of_d.male<-of_tp.male$tp



# female OF (background mortality) -----
of_tp.female<-working.lifetable %>% 
     filter(age %in% 
            seq(input$av_age_other.female, input$av_age_other.female+4))
# to monthly 
of_tp.female<-data.frame(
  t=c(0,12,24, 36,48, 60),
  tp=c(NA, of_tp.female$female))

of_tp.female<-data.frame(time=1:60, 
  tp=c(rep(tp.different.cycle.length(of_tp.female$tp[2],12),12),
       rep(tp.different.cycle.length(of_tp.female$tp[3],12),12),
       rep(tp.different.cycle.length(of_tp.female$tp[4],12),12),
       rep(tp.different.cycle.length(of_tp.female$tp[5],12),12),
       rep(tp.different.cycle.length(of_tp.female$tp[6],12),12)))
  

background.tps$tp.of_d.female<-of_tp.female$tp
  
#hf1 male mortality-----
hf1_tp.male<-data.frame(
  t=c(0,3,12, 60),
  tp=c(NA, 
       input$prob.death_hf1_0_3.male, # 0 to 3 months
       input$prob.death_hf1_0_12.male-input$prob.death_hf1_0_3.male,# 3 to 12 months
       input$prob.death_hf1_0_60.male-input$prob.death_hf1_0_12.male))# 12 to 60 months

hf1_tp.male<-data.frame(time=1:60, 
  tp=c(rep(tp.different.cycle.length(hf1_tp.male$tp[2],3),3),
       rep(tp.different.cycle.length(hf1_tp.male$tp[3],9),9),
       rep(tp.different.cycle.length(hf1_tp.male$tp[4],48),48)))
  
background.tps$tp.hf1_d.male<-hf1_tp.male$tp

#hf1 female mortality-----
hf1_tp.female<-data.frame(
  t=c(0,3,12, 60),
  tp=c(NA, 
       input$prob.death_hf1_0_3.female, # 0 to 3 months
       input$prob.death_hf1_0_12.female-input$prob.death_hf1_0_3.female,# 3 to 12 months
       input$prob.death_hf1_0_60.female-input$prob.death_hf1_0_12.female))# 12 to 60 months

hf1_tp.female<-data.frame(time=1:60, 
  tp=c(rep(tp.different.cycle.length(hf1_tp.female$tp[2],3),3),
       rep(tp.different.cycle.length(hf1_tp.female$tp[3],9),9),
       rep(tp.different.cycle.length(hf1_tp.female$tp[4],48),48)))

background.tps$tp.hf1_d.female<-hf1_tp.female$tp


#hf2 male mortality-----
hf2_tp.male<-data.frame(
  t=c(0,3,12, 60),
  tp=c(NA, 
       input$prob.death_hf2_0_3.male, # 0 to 3 months
       input$prob.death_hf2_0_12.male-input$prob.death_hf2_0_3.male,# 3 to 12 months
       input$prob.death_hf2_0_60.male-input$prob.death_hf2_0_12.male))# 12 to 60 months


hf2_tp.male<-data.frame(time=1:60, 
  tp=c(rep(tp.different.cycle.length(hf2_tp.male$tp[2],3),3),
       rep(tp.different.cycle.length(hf2_tp.male$tp[3],9),9),
       rep(tp.different.cycle.length(hf2_tp.male$tp[4],48),48)))
  
background.tps$tp.hf2_d.male<-hf2_tp.male$tp


#hf2 female mortality-----
hf2_tp.female<-data.frame(
  t=c(0,3,12, 60),
  tp=c(NA, 
       input$prob.death_hf2_0_3.female, # 0 to 3 months
       input$prob.death_hf2_0_12.female-input$prob.death_hf2_0_3.female,# 3 to 12 months
       input$prob.death_hf2_0_60.female-input$prob.death_hf2_0_12.female))# 12 to 60 months


hf2_tp.female<-data.frame(time=1:60, 
  tp=c(rep(tp.different.cycle.length(hf2_tp.female$tp[2],3),3),
       rep(tp.different.cycle.length(hf2_tp.female$tp[3],9),9),
       rep(tp.different.cycle.length(hf2_tp.female$tp[4],48),48)))

  
background.tps$tp.hf2_d.female<-hf2_tp.female$tp



#sf male mortality-----
sf_tp.male<-data.frame(
  t=c(0,3,12, 60),
    tp=c(NA, 
       input$prob.death_sf_0_3.male, # 0 to 3 months
       input$prob.death_sf_0_12.male-input$prob.death_sf_0_3.male,# 3 to 12 months
       input$prob.death_sf_0_60.male-input$prob.death_sf_0_12.male))# 12 to 60 months

sf_tp.male<-data.frame(time=1:60, 
  tp=c(rep(tp.different.cycle.length(sf_tp.male$tp[2],3),3),
       rep(tp.different.cycle.length(sf_tp.male$tp[3],9),9),
       rep(tp.different.cycle.length(sf_tp.male$tp[4],48),48)))
  
background.tps$tp.sf_d.male<-sf_tp.male$tp


#sf female mortality-----
sf_tp.female<-data.frame(
  t=c(0,3,12, 60),
    tp=c(NA, 
       input$prob.death_sf_0_3.female, # 0 to 3 months
       input$prob.death_sf_0_12.female-input$prob.death_sf_0_3.female,# 3 to 12 months
       input$prob.death_sf_0_60.female-input$prob.death_sf_0_12.female))# 12 to 60 months

sf_tp.female<-data.frame(time=1:60, 
  tp=c(rep(tp.different.cycle.length(sf_tp.female$tp[2],3),3),
       rep(tp.different.cycle.length(sf_tp.female$tp[3],9),9),
       rep(tp.different.cycle.length(sf_tp.female$tp[4],48),48)))
  
background.tps$tp.sf_d.female<-sf_tp.female$tp



#out ----
background.tps
})


get.background.tps.long<-reactive({
get.background.tps() %>%
 pivot_longer(-time, names_to = "tp", values_to = "count")
})



get.plots.tp<-reactive({
  
background.tps_long<- get.background.tps.long()
  
tp.gender<-input$input.tp.plot.gender
tp.starting.state<-input$input.tp.plot.fx

if(tp.gender=="Male" & tp.starting.state=="Hip"){
  plot.data<-background.tps_long %>% 
    filter(tp %in% 
          c("tp.hf_hf.male", "tp.hf_of.male",
            "tp.hf_sf.male", "tp.hf1_d.male", "tp.hf2_d.male")) %>% 
    mutate(tp=ifelse(tp=="tp.hf_hf.male", "Hip fracture after hip fracture",
              ifelse(tp=="tp.hf_of.male", "Other fracture after hip fracture",
              ifelse(tp=="tp.hf_sf.male", "Spine fracture after hip fracture",
              ifelse(tp=="tp.hf1_d.male", "Death after first hip fracture",
              ifelse(tp=="tp.hf2_d.male", "Death after second hip fracture", NA))))))
  
  } 
if(tp.gender=="Male" & tp.starting.state=="Spine"){
  plot.data<-background.tps_long %>% 
    filter(tp %in% 
          c("tp.sf_hf.male", "tp.sf_of.male",
            "tp.sf_sf.male", "tp.sf_d.male")) %>% 
    mutate(tp=ifelse(tp=="tp.sf_hf.male", "Hip fracture after spine fracture",
              ifelse(tp=="tp.sf_of.male", "Other fracture after spine fracture",
              ifelse(tp=="tp.sf_sf.male", "Spine fracture after spine fracture",
              ifelse(tp=="tp.sf_d.male", "Death after spine fracture", NA)))))
  
  } 
if(tp.gender=="Male" & tp.starting.state=="Other"){
  plot.data<-background.tps_long %>% 
    filter(tp %in% 
          c("tp.of_hf.male", "tp.of_of.male",
            "tp.of_sf.male", "tp.of_d.male")) %>% 
    mutate(tp=ifelse(tp=="tp.of_hf.male", "Hip fracture after other fracture",
              ifelse(tp=="tp.of_of.male", "Other fracture after other fracture",
              ifelse(tp=="tp.of_sf.male", "Spine fracture after other fracture",
              ifelse(tp=="tp.of_d.male", "Death after other fracture", NA)))))
  
  } 

if(tp.gender=="Female" & tp.starting.state=="Hip"){
  plot.data<-background.tps_long %>% 
    filter(tp %in% 
          c("tp.hf_hf.female", "tp.hf_of.female",
            "tp.hf_sf.female", "tp.hf1_d.female", "tp.hf2_d.female")) %>% 
    mutate(tp=ifelse(tp=="tp.hf_hf.female", "Hip fracture after hip fracture",
              ifelse(tp=="tp.hf_of.female", "Other fracture after hip fracture",
              ifelse(tp=="tp.hf_sf.female", "Spine fracture after hip fracture",
              ifelse(tp=="tp.hf1_d.female", "Death after first hip fracture",
              ifelse(tp=="tp.hf2_d.female", "Death after second hip fracture", NA))))))
  
  } 
if(tp.gender=="Female" & tp.starting.state=="Spine"){
  plot.data<-background.tps_long %>% 
    filter(tp %in% 
          c("tp.sf_hf.female", "tp.sf_of.female",
            "tp.sf_sf.female", "tp.sf_d.female")) %>% 
    mutate(tp=ifelse(tp=="tp.sf_hf.female", "Hip fracture after spine fracture",
              ifelse(tp=="tp.sf_of.female", "Other fracture after spine fracture",
              ifelse(tp=="tp.sf_sf.female", "Spine fracture after spine fracture",
              ifelse(tp=="tp.sf_d.female", "Death after spine fracture", NA)))))
  
  } 
if(tp.gender=="Female" & tp.starting.state=="Other"){
  plot.data<-background.tps_long %>% 
    filter(tp %in% 
          c("tp.of_hf.female", "tp.of_of.female",
            "tp.of_sf.female", "tp.of_d.female")) %>% 
    mutate(tp=ifelse(tp=="tp.of_hf.female", "Hip fracture after other fracture",
              ifelse(tp=="tp.of_of.female", "Other fracture after other fracture",
              ifelse(tp=="tp.of_sf.female", "Spine fracture after other fracture",
              ifelse(tp=="tp.of_d.female", "Death after other fracture", NA)))))
  
  } 

  plot.data
  
})

output$plot.tps<- renderPlot({
  
  get.plots.tp() %>% 
  ggplot() +
  facet_wrap(.~tp, ncol=1)+
  geom_point(aes(time, count))+
  theme_bw()+
  xlab("Month") +
  ylab("Transition probability")+ 
  scale_y_continuous(labels = scales::percent_format())+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=9, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=12),
        legend.position="top")


 }, height=900, res = 100 )

# transition matricies -----
lapply(c(
  "qol.pre.hip", "qol.post.hip", "qol.post.4.month.hip",
  "qol.pre.spine", "qol.post.spine", "qol.post.4.month.spine",
  "qol.pre.other", "qol.post.other", "qol.post.4.month.other",
         "no.fls.prob_identification.hip",
         "no.fls.prob_identification.spine",
         "no.fls.prob_identification.other",
         "fls.prob_identification.hip",
         "fls.prob_identification.spine",
         "fls.prob_identification.other",
         "no.fls.time_to_treat.hip",
         "no.fls.time_to_treat.spine",
         "no.fls.time_to_treat.other",
         "fls.time_to_treat.hip",
         "fls.time_to_treat.spine",
         "fls.time_to_treat.other",
         
         "no.fls.trt.spine.male",
         "no.fls.trt.hip.male",
         "no.fls.trt.other.male",
         "no.fls.trt.spine.female",
         "no.fls.trt.hip.female",
         "no.fls.trt.other.female",
         "fls.trt.spine.male",
         "fls.trt.hip.male",
         "fls.trt.other.male",
         "fls.trt.spine.female",
         "fls.trt.hip.female",
         "fls.trt.other.female",
         "no.fls.trt.alendronate.spine.male",
            "no.fls.trt.risedronate.spine.male",
            "no.fls.trt.strontium.spine.male",
            "no.fls.trt.ibandronate.spine.male",
            "no.fls.trt.raloxifene.spine.male",
            "no.fls.trt.denosumab.spine.male",
            "no.fls.trt.zoledronate.spine.male",
            "no.fls.trt.teriparatide.spine.male",
            "no.fls.trt.abaloparatide.spine.male",
            "no.fls.trt.romo.spine.male",
                  "no.fls.trt.alendronate.hip.male",
            "no.fls.trt.risedronate.hip.male",
            "no.fls.trt.strontium.hip.male",
            "no.fls.trt.ibandronate.hip.male",
             "no.fls.trt.raloxifene.hip.male",
            "no.fls.trt.denosumab.hip.male",
            "no.fls.trt.zoledronate.hip.male",
            "no.fls.trt.teriparatide.hip.male",
            "no.fls.trt.abaloparatide.hip.male",
            "no.fls.trt.romo.hip.male",
                  "no.fls.trt.alendronate.other.male",
            "no.fls.trt.risedronate.other.male",
            "no.fls.trt.strontium.other.male",
            "no.fls.trt.ibandronate.other.male",
            "no.fls.trt.raloxifene.other.male",
            "no.fls.trt.denosumab.other.male",
            "no.fls.trt.zoledronate.other.male",
            "no.fls.trt.teriparatide.other.male",
            "no.fls.trt.abaloparatide.other.male",
            "no.fls.trt.romo.other.male",
         
                  "no.fls.trt.alendronate.spine.female",
            "no.fls.trt.risedronate.spine.female",
            "no.fls.trt.strontium.spine.female",
            "no.fls.trt.ibandronate.spine.female",
            "no.fls.trt.raloxifene.spine.female",
            "no.fls.trt.denosumab.spine.female",
            "no.fls.trt.zoledronate.spine.female",
            "no.fls.trt.teriparatide.spine.female",
            "no.fls.trt.abaloparatide.spine.female",
            "no.fls.trt.romo.spine.female",
                  "no.fls.trt.alendronate.hip.female",
            "no.fls.trt.risedronate.hip.female",
            "no.fls.trt.strontium.hip.female",
            "no.fls.trt.ibandronate.hip.female",
            "no.fls.trt.raloxifene.hip.female",
            "no.fls.trt.denosumab.hip.female",
            "no.fls.trt.zoledronate.hip.female",
            "no.fls.trt.teriparatide.hip.female",
            "no.fls.trt.abaloparatide.hip.female",
            "no.fls.trt.romo.hip.female",
                  "no.fls.trt.alendronate.other.female",
            "no.fls.trt.risedronate.other.female",
            "no.fls.trt.strontium.other.female",
            "no.fls.trt.ibandronate.other.female",
            "no.fls.trt.raloxifene.other.female",
            "no.fls.trt.denosumab.other.female",
            "no.fls.trt.zoledronate.other.female",
            "no.fls.trt.teriparatide.other.female",
            "no.fls.trt.abaloparatide.other.female",
            "no.fls.trt.romo.other.female",
         
                  "fls.trt.alendronate.spine.male",
            "fls.trt.risedronate.spine.male",
            "fls.trt.strontium.spine.male",
            "fls.trt.ibandronate.spine.male",
            "fls.trt.raloxifene.spine.male",
            "fls.trt.denosumab.spine.male",
            "fls.trt.zoledronate.spine.male",
            "fls.trt.teriparatide.spine.male",
            "fls.trt.abaloparatide.spine.male",
            "fls.trt.romo.spine.male",
                  "fls.trt.alendronate.hip.male",
            "fls.trt.risedronate.hip.male",
            "fls.trt.strontium.hip.male",
            "fls.trt.ibandronate.hip.male",
            "fls.trt.raloxifene.hip.male",
            "fls.trt.denosumab.hip.male",
            "fls.trt.zoledronate.hip.male",
            "fls.trt.teriparatide.hip.male",
            "fls.trt.abaloparatide.hip.male",
            "fls.trt.romo.hip.male",
                  "fls.trt.alendronate.other.male",
            "fls.trt.risedronate.other.male",
            "fls.trt.strontium.other.male",
            "fls.trt.ibandronate.other.male",
            "fls.trt.raloxifene.other.male",
            "fls.trt.denosumab.other.male",
            "fls.trt.zoledronate.other.male",
            "fls.trt.teriparatide.other.male",
            "fls.trt.abaloparatide.other.male",
            "fls.trt.romo.other.male",
         
                  "fls.trt.alendronate.spine.female",
            "fls.trt.risedronate.spine.female",
            "fls.trt.strontium.spine.female",
            "fls.trt.ibandronate.spine.female",
            "fls.trt.raloxifene.spine.female",
            "fls.trt.denosumab.spine.female",
            "fls.trt.zoledronate.spine.female",
            "fls.trt.teriparatide.spine.female",
            "fls.trt.abaloparatide.spine.female",
            "fls.trt.romo.spine.female",
                  "fls.trt.alendronate.hip.female",
            "fls.trt.risedronate.hip.female",
            "fls.trt.strontium.hip.female",
            "fls.trt.ibandronate.hip.female",
            "fls.trt.raloxifene.hip.female",
            "fls.trt.denosumab.hip.female",
            "fls.trt.zoledronate.hip.female",
            "fls.trt.teriparatide.hip.female",
            "fls.trt.abaloparatide.hip.female",
            "fls.trt.romo.hip.female",
                  "fls.trt.alendronate.other.female",
            "fls.trt.risedronate.other.female",
            "fls.trt.strontium.other.female",
            "fls.trt.ibandronate.other.female",
            "fls.trt.raloxifene.other.female",
            "fls.trt.denosumab.other.female",
            "fls.trt.zoledronate.other.female",
            "fls.trt.teriparatide.other.female",
            "fls.trt.abaloparatide.other.female",
            "fls.trt.romo.other.female",
         "primary.adh_alendronate.spine.no.fls.male",
"primary.adh_risedronate.spine.no.fls.male",
"primary.adh_strontium.spine.no.fls.male",
"primary.adh_ibandronate.spine.no.fls.male",
"primary.adh_raloxifene.spine.no.fls.male",
"primary.adh_denosumab.spine.no.fls.male",
"primary.adh_zoledronate.spine.no.fls.male",
"primary.adh_teriparatide.spine.no.fls.male",
"primary.adh_abaloparatide.spine.no.fls.male",
"primary.adh_romo.spine.no.fls.male",
         "primary.adh_alendronate.hip.no.fls.male",
"primary.adh_risedronate.hip.no.fls.male",
"primary.adh_strontium.hip.no.fls.male",
"primary.adh_ibandronate.hip.no.fls.male",
"primary.adh_raloxifene.hip.no.fls.male",
"primary.adh_denosumab.hip.no.fls.male",
"primary.adh_zoledronate.hip.no.fls.male",
"primary.adh_teriparatide.hip.no.fls.male",
"primary.adh_abaloparatide.hip.no.fls.male",
"primary.adh_romo.hip.no.fls.male",
         "primary.adh_alendronate.other.no.fls.male",
"primary.adh_risedronate.other.no.fls.male",
"primary.adh_strontium.other.no.fls.male",
"primary.adh_ibandronate.other.no.fls.male",
"primary.adh_raloxifene.other.no.fls.male",
"primary.adh_denosumab.other.no.fls.male",
"primary.adh_zoledronate.other.no.fls.male",
"primary.adh_teriparatide.other.no.fls.male",
"primary.adh_abaloparatide.other.no.fls.male",
"primary.adh_romo.other.no.fls.male",

         "primary.adh_alendronate.spine.no.fls.female",
"primary.adh_risedronate.spine.no.fls.female",
"primary.adh_strontium.spine.no.fls.female",
"primary.adh_ibandronate.spine.no.fls.female",
"primary.adh_raloxifene.spine.no.fls.female",
"primary.adh_denosumab.spine.no.fls.female",
"primary.adh_zoledronate.spine.no.fls.female",
"primary.adh_teriparatide.spine.no.fls.female",
"primary.adh_abaloparatide.spine.no.fls.female",
"primary.adh_romo.spine.no.fls.female",
         "primary.adh_alendronate.hip.no.fls.female",
"primary.adh_risedronate.hip.no.fls.female",
"primary.adh_strontium.hip.no.fls.female",
"primary.adh_ibandronate.hip.no.fls.female",
"primary.adh_raloxifene.hip.no.fls.female",
"primary.adh_denosumab.hip.no.fls.female",
"primary.adh_zoledronate.hip.no.fls.female",
"primary.adh_teriparatide.hip.no.fls.female",
"primary.adh_abaloparatide.hip.no.fls.female",
"primary.adh_romo.hip.no.fls.female",
         "primary.adh_alendronate.other.no.fls.female",
"primary.adh_risedronate.other.no.fls.female",
"primary.adh_strontium.other.no.fls.female",
"primary.adh_ibandronate.other.no.fls.female",
"primary.adh_raloxifene.other.no.fls.female",
"primary.adh_denosumab.other.no.fls.female",
"primary.adh_zoledronate.other.no.fls.female",
"primary.adh_teriparatide.other.no.fls.female",
"primary.adh_abaloparatide.other.no.fls.female",
"primary.adh_romo.other.no.fls.female",





         "primary.adh_alendronate.spine.fls.male",
"primary.adh_risedronate.spine.fls.male",
"primary.adh_strontium.spine.fls.male",
"primary.adh_ibandronate.spine.fls.male",
"primary.adh_raloxifene.spine.fls.male",
"primary.adh_denosumab.spine.fls.male",
"primary.adh_zoledronate.spine.fls.male",
"primary.adh_teriparatide.spine.fls.male",
"primary.adh_abaloparatide.spine.fls.male",
"primary.adh_romo.spine.fls.male",
         "primary.adh_alendronate.hip.fls.male",
"primary.adh_risedronate.hip.fls.male",
"primary.adh_strontium.hip.fls.male",
"primary.adh_ibandronate.hip.fls.male",
"primary.adh_raloxifene.hip.fls.male",
"primary.adh_denosumab.hip.fls.male",
"primary.adh_zoledronate.hip.fls.male",
"primary.adh_teriparatide.hip.fls.male",
"primary.adh_abaloparatide.hip.fls.male",
"primary.adh_romo.hip.fls.male",
         "primary.adh_alendronate.other.fls.male",
"primary.adh_risedronate.other.fls.male",
"primary.adh_strontium.other.fls.male",
"primary.adh_ibandronate.other.fls.male",
"primary.adh_raloxifene.other.fls.male",
"primary.adh_denosumab.other.fls.male",
"primary.adh_zoledronate.other.fls.male",
"primary.adh_teriparatide.other.fls.male",
"primary.adh_abaloparatide.other.fls.male",
"primary.adh_romo.other.fls.male",

         "primary.adh_alendronate.spine.fls.female",
"primary.adh_risedronate.spine.fls.female",
"primary.adh_strontium.spine.fls.female",
"primary.adh_ibandronate.spine.fls.female",
"primary.adh_raloxifene.spine.fls.female",
"primary.adh_denosumab.spine.fls.female",
"primary.adh_zoledronate.spine.fls.female",
"primary.adh_teriparatide.spine.fls.female",
"primary.adh_abaloparatide.spine.fls.female",
"primary.adh_romo.spine.fls.female",
         "primary.adh_alendronate.hip.fls.female",
"primary.adh_risedronate.hip.fls.female",
"primary.adh_strontium.hip.fls.female",
"primary.adh_ibandronate.hip.fls.female",
"primary.adh_raloxifene.hip.fls.female",
"primary.adh_denosumab.hip.fls.female",
"primary.adh_zoledronate.hip.fls.female",
"primary.adh_teriparatide.hip.fls.female",
"primary.adh_abaloparatide.hip.fls.female",
"primary.adh_romo.hip.fls.female",
         "primary.adh_alendronate.other.fls.female",
"primary.adh_risedronate.other.fls.female",
"primary.adh_strontium.other.fls.female",
"primary.adh_ibandronate.other.fls.female",
"primary.adh_raloxifene.other.fls.female",
"primary.adh_denosumab.other.fls.female",
"primary.adh_zoledronate.other.fls.female",
"primary.adh_teriparatide.other.fls.female",
"primary.adh_abaloparatide.other.fls.female",
"primary.adh_romo.other.fls.female",


"romo.to.nothing.no.fls.male",
"romo.to.alendronate.no.fls.male",
"romo.to.risedronate.no.fls.male",
"romo.to.strontium.no.fls.male",
"romo.to.ibandronate.no.fls.male",
"romo.to.raloxifene.no.fls.male",
"romo.to.denosumab.no.fls.male",
"romo.to.zoledronate.no.fls.male",
"romo.to.nothing.no.fls.female",
"romo.to.alendronate.no.fls.female",
"romo.to.risedronate.no.fls.female",
"romo.to.strontium.no.fls.female",
"romo.to.ibandronate.no.fls.female",
"romo.to.raloxifene.no.fls.female",
"romo.to.denosumab.no.fls.female",
"romo.to.zoledronate.no.fls.female",
"romo.to.nothing.fls.male",
"romo.to.alendronate.fls.male",
"romo.to.risedronate.fls.male",
"romo.to.strontium.fls.male",
"romo.to.ibandronate.fls.male",
"romo.to.raloxifene.fls.male",
"romo.to.denosumab.fls.male",
"romo.to.zoledronate.fls.male",
"romo.to.nothing.fls.female",
"romo.to.alendronate.fls.female",
"romo.to.risedronate.fls.female",
"romo.to.strontium.fls.female",
"romo.to.ibandronate.fls.female",
"romo.to.raloxifene.fls.female",
"romo.to.denosumab.fls.female",
"romo.to.zoledronate.fls.female",

"abaloparatide.to.nothing.no.fls.male",
"abaloparatide.to.alendronate.no.fls.male",
"abaloparatide.to.risedronate.no.fls.male",
"abaloparatide.to.strontium.no.fls.male",
"abaloparatide.to.ibandronate.no.fls.male",
"abaloparatide.to.raloxifene.no.fls.male",
"abaloparatide.to.denosumab.no.fls.male",
"abaloparatide.to.zoledronate.no.fls.male",
"abaloparatide.to.nothing.no.fls.female",
"abaloparatide.to.alendronate.no.fls.female",
"abaloparatide.to.risedronate.no.fls.female",
"abaloparatide.to.strontium.no.fls.female",
"abaloparatide.to.ibandronate.no.fls.female",
"abaloparatide.to.raloxifene.no.fls.female",
"abaloparatide.to.denosumab.no.fls.female",
"abaloparatide.to.zoledronate.no.fls.female",
"abaloparatide.to.nothing.fls.male",
"abaloparatide.to.alendronate.fls.male",
"abaloparatide.to.risedronate.fls.male",
"abaloparatide.to.strontium.fls.male",
"abaloparatide.to.ibandronate.fls.male",
"abaloparatide.to.raloxifene.fls.male",
"abaloparatide.to.denosumab.fls.male",
"abaloparatide.to.zoledronate.fls.male",
"abaloparatide.to.nothing.fls.female",
"abaloparatide.to.alendronate.fls.female",
"abaloparatide.to.risedronate.fls.female",
"abaloparatide.to.strontium.fls.female",
"abaloparatide.to.ibandronate.fls.female",
"abaloparatide.to.raloxifene.fls.female",
"abaloparatide.to.denosumab.fls.female",
"abaloparatide.to.zoledronate.fls.female",

"teriparatide.to.nothing.no.fls.male",
"teriparatide.to.alendronate.no.fls.male",
"teriparatide.to.risedronate.no.fls.male",
"teriparatide.to.strontium.no.fls.male",
"teriparatide.to.ibandronate.no.fls.male",
"teriparatide.to.raloxifene.no.fls.male",
"teriparatide.to.denosumab.no.fls.male",
"teriparatide.to.zoledronate.no.fls.male",
"teriparatide.to.nothing.no.fls.female",
"teriparatide.to.alendronate.no.fls.female",
"teriparatide.to.risedronate.no.fls.female",
"teriparatide.to.strontium.no.fls.female",
"teriparatide.to.ibandronate.no.fls.female",
"teriparatide.to.raloxifene.no.fls.female",
"teriparatide.to.denosumab.no.fls.female",
"teriparatide.to.zoledronate.no.fls.female",
"teriparatide.to.nothing.fls.male",
"teriparatide.to.alendronate.fls.male",
"teriparatide.to.risedronate.fls.male",
"teriparatide.to.strontium.fls.male",
"teriparatide.to.ibandronate.fls.male",
"teriparatide.to.raloxifene.fls.male",
"teriparatide.to.denosumab.fls.male",
"teriparatide.to.zoledronate.fls.male",
"teriparatide.to.nothing.fls.female",
"teriparatide.to.alendronate.fls.female",
"teriparatide.to.risedronate.fls.female",
"teriparatide.to.strontium.fls.female",
"teriparatide.to.ibandronate.fls.female",
"teriparatide.to.raloxifene.fls.female",
"teriparatide.to.denosumab.fls.female",
"teriparatide.to.zoledronate.fls.female",

"no.fls.monitored.spine.4m.male",
"no.fls.monitored.hip.4m.male",
"no.fls.monitored.other.4m.male",
"no.fls.monitored.spine.12m.male",
"no.fls.monitored.hip.12m.male",
"no.fls.monitored.other.12m.male",
"fls.monitored.spine.4m.male",
"fls.monitored.hip.4m.male",
"fls.monitored.other.4m.male",
"fls.monitored.spine.12m.male",
"fls.monitored.hip.12m.male",
"fls.monitored.other.12m.male",
"no.fls.monitored.spine.4m.female",
"no.fls.monitored.hip.4m.female",
"no.fls.monitored.other.4m.female",
"no.fls.monitored.spine.12m.female",
"no.fls.monitored.hip.12m.female",
"no.fls.monitored.other.12m.female",
"fls.monitored.spine.4m.female",
"fls.monitored.hip.4m.female",
"fls.monitored.other.4m.female",
"fls.monitored.spine.12m.female",
"fls.monitored.hip.12m.female",
"fls.monitored.other.12m.female",

"not.monitored.4m_adh_alendronate.male",
"not.monitored.4m_adh_risedronate.male",
"not.monitored.4m_adh_strontium.male",
"not.monitored.4m_adh_ibandronate.male",
"not.monitored.4m_adh_raloxifene.male",
"not.monitored.4m_adh_denosumab.male",
"not.monitored.4m_adh_zoledronate.male",
"not.monitored.4m_adh_teriparatide.male",
"not.monitored.4m_adh_abaloparatide.male",
"not.monitored.4m_adh_romo.male",
"not.monitored.12m_adh_alendronate.male",
"not.monitored.12m_adh_risedronate.male",
"not.monitored.12m_adh_strontium.male",
"not.monitored.12m_adh_ibandronate.male",
"not.monitored.12m_adh_raloxifene.male",
"not.monitored.12m_adh_denosumab.male",
"not.monitored.12m_adh_zoledronate.male",
"not.monitored.12m_adh_teriparatide.male",
"not.monitored.12m_adh_abaloparatide.male",
"not.monitored.4m_adh_alendronate.female",
"not.monitored.4m_adh_risedronate.female",
"not.monitored.4m_adh_strontium.female",
"not.monitored.4m_adh_ibandronate.female",
"not.monitored.4m_adh_raloxifene.female",
"not.monitored.4m_adh_denosumab.female",
"not.monitored.4m_adh_zoledronate.female",
"not.monitored.4m_adh_teriparatide.female",
"not.monitored.4m_adh_abaloparatide.female",
"not.monitored.4m_adh_romo.female",
"not.monitored.12m_adh_alendronate.female",
"not.monitored.12m_adh_risedronate.female",
"not.monitored.12m_adh_strontium.female",
"not.monitored.12m_adh_ibandronate.female",
"not.monitored.12m_adh_raloxifene.female",
"not.monitored.12m_adh_denosumab.female",
"not.monitored.12m_adh_zoledronate.female",
"not.monitored.12m_adh_teriparatide.female",
"not.monitored.12m_adh_abaloparatide.female",
"monitored.4m_adh_alendronate.male",
"monitored.4m_adh_risedronate.male",
"monitored.4m_adh_strontium.male",
"monitored.4m_adh_ibandronate.male",
"monitored.4m_adh_raloxifene.male",
"monitored.4m_adh_denosumab.male",
"monitored.4m_adh_zoledronate.male",
"monitored.4m_adh_teriparatide.male",
"monitored.4m_adh_abaloparatide.male",
"monitored.4m_adh_romo.male",
"monitored.12m_adh_alendronate.male",
"monitored.12m_adh_risedronate.male",
"monitored.12m_adh_strontium.male",
"monitored.12m_adh_ibandronate.male",
"monitored.12m_adh_raloxifene.male",
"monitored.12m_adh_denosumab.male",
"monitored.12m_adh_zoledronate.male",
"monitored.12m_adh_teriparatide.male",
"monitored.12m_adh_abaloparatide.male",
"monitored.4m_adh_alendronate.female",
"monitored.4m_adh_risedronate.female",
"monitored.4m_adh_strontium.female",
"monitored.4m_adh_ibandronate.female",
"monitored.4m_adh_raloxifene.female",
"monitored.4m_adh_denosumab.female",
"monitored.4m_adh_zoledronate.female",
"monitored.4m_adh_teriparatide.female",
"monitored.4m_adh_abaloparatide.female",
"monitored.4m_adh_romo.female",
"monitored.12m_adh_alendronate.female",
"monitored.12m_adh_risedronate.female",
"monitored.12m_adh_strontium.female",
"monitored.12m_adh_ibandronate.female",
"monitored.12m_adh_raloxifene.female",
"monitored.12m_adh_denosumab.female",
"monitored.12m_adh_zoledronate.female",
"monitored.12m_adh_teriparatide.female",
"monitored.12m_adh_abaloparatide.female",

"adh_annual_decline_alendronate.no.fls.male",
"adh_annual_decline_risedronate.no.fls.male",
"adh_annual_decline_strontium.no.fls.male",
"adh_annual_decline_ibandronate.no.fls.male",
"adh_annual_decline_raloxifene.no.fls.male",
"adh_annual_decline_denosumab.no.fls.male",
"adh_annual_decline_zoledronate.no.fls.male",
"adh_annual_decline_alendronate.no.fls.female",
"adh_annual_decline_risedronate.no.fls.female",
"adh_annual_decline_strontium.no.fls.female",
"adh_annual_decline_ibandronate.no.fls.female",
"adh_annual_decline_raloxifene.no.fls.female",
"adh_annual_decline_denosumab.no.fls.female",
"adh_annual_decline_zoledronate.no.fls.female",
"adh_annual_decline_alendronate.fls.male",
"adh_annual_decline_risedronate.fls.male",
"adh_annual_decline_strontium.fls.male",
"adh_annual_decline_ibandronate.fls.male",
"adh_annual_decline_raloxifene.fls.male",
"adh_annual_decline_denosumab.fls.male",
"adh_annual_decline_zoledronate.fls.male",
"adh_annual_decline_alendronate.fls.female",
"adh_annual_decline_risedronate.fls.female",
"adh_annual_decline_strontium.fls.female",
"adh_annual_decline_ibandronate.fls.female",
"adh_annual_decline_raloxifene.fls.female",
"adh_annual_decline_denosumab.fls.female",
"adh_annual_decline_zoledronate.fls.female",

###

"prop.admitted.surgery.hip",
"prop.admitted.no.surgery.hip",
"prop.not.admitted.clinic.hip",

"prop.hospital.spine",
"prop.hospital.community.spine",
"prop.hospital.spine.kyphoplasty",
"prop.hospital.spine.vertebroplasty",
"prop.hospital.spine.no.intervention",
"prop.not.admitted.spine.kyphoplasty",
"prop.not.admitted.spine.vertebroplasty",
"prop.not.admitted.spine.no.intervention",

"prop.admitted.surgery.other",
"prop.admitted.no.surgery.other",
"prop.not.admitted.clinic.other",

"hospital.los.hip.surg",
"hospital.los.hip.no.surg",
"hospital.los.spine.kyphoplasty",
"hospital.los.spine.vertebroplasty",
"hospital.los.spine.no.surg",
"hospital.los.other.surg",
"hospital.los.other.no.surg",

"visits.comm.consults.spine",

"cost.a_e.visit",
"cost.hosp.per.day",
"cost.hosp.clinic.visit",

"cost.hip.surg",
"cost.spine.kyphoplasty",
"cost.spine.vertebroplasty",
"cost.other.surg",

"cost.spine.community.care",


"no.fls.administrator.identification.mins.hip",
"no.fls.administrator.assessment.mins.hip",
"no.fls.administrator.recommendation.mins.hip",
"no.fls.administrator.monitoring.mins.hip",
"fls.administrator.identification.mins.hip",
"fls.administrator.assessment.mins.hip",
"fls.administrator.recommendation.mins.hip",
"fls.administrator.monitoring.mins.hip",
"fls.fls_coordinator.identification.mins.hip",
"fls.fls_coordinator.assessment.mins.hip",
"fls.fls_coordinator.recommendation.mins.hip",
"fls.fls_coordinator.monitoring.mins.hip",
"no.fls.nurse.identification.mins.hip",
"no.fls.nurse.assessment.mins.hip",
"no.fls.nurse.recommendation.mins.hip",
"no.fls.nurse.monitoring.mins.hip",
"fls.nurse.identification.mins.hip",
"fls.nurse.assessment.mins.hip",
"fls.nurse.recommendation.mins.hip",
"fls.nurse.monitoring.mins.hip",
"no.fls.doctor.identification.mins.hip",
"no.fls.doctor.assessment.mins.hip",
"no.fls.doctor.recommendation.mins.hip",
"no.fls.doctor.monitoring.mins.hip",
"fls.doctor.identification.mins.hip",
"fls.doctor.assessment.mins.hip",
"fls.doctor.recommendation.mins.hip",
"fls.doctor.monitoring.mins.hip",
"no.fls.radiographer.identification.mins.hip",
"no.fls.radiographer.assessment.mins.hip",
"no.fls.radiographer.recommendation.mins.hip",
"no.fls.radiographer.monitoring.mins.hip",
"fls.radiographer.identification.mins.hip",
"fls.radiographer.assessment.mins.hip",
"fls.radiographer.recommendation.mins.hip",
"fls.radiographer.monitoring.mins.hip",
"no.fls.allied_health.identification.mins.hip",
"no.fls.allied_health.assessment.mins.hip",
"no.fls.allied_health.recommendation.mins.hip",
"no.fls.allied_health.monitoring.mins.hip",
"fls.allied_health.identification.mins.hip",
"fls.allied_health.assessment.mins.hip",
"fls.allied_health.recommendation.mins.hip",
"fls.allied_health.monitoring.mins.hip",
"no.fls.other.identification.mins.hip",
"no.fls.other.assessment.mins.hip",
"no.fls.other.recommendation.mins.hip",
"no.fls.other.monitoring.mins.hip",
"fls.other.identification.mins.hip",
"fls.other.assessment.mins.hip",
"fls.other.recommendation.mins.hip",
"fls.other.monitoring.mins.hip",

"no.fls.administrator.identification.mins.spine",
"no.fls.administrator.assessment.mins.spine",
"no.fls.administrator.recommendation.mins.spine",
"no.fls.administrator.monitoring.mins.spine",
"fls.administrator.identification.mins.spine",
"fls.administrator.assessment.mins.spine",
"fls.administrator.recommendation.mins.spine",
"fls.administrator.monitoring.mins.spine",
"fls.fls_coordinator.identification.mins.spine",
"fls.fls_coordinator.assessment.mins.spine",
"fls.fls_coordinator.recommendation.mins.spine",
"fls.fls_coordinator.monitoring.mins.spine",
"no.fls.nurse.identification.mins.spine",
"no.fls.nurse.assessment.mins.spine",
"no.fls.nurse.recommendation.mins.spine",
"no.fls.nurse.monitoring.mins.spine",
"fls.nurse.identification.mins.spine",
"fls.nurse.assessment.mins.spine",
"fls.nurse.recommendation.mins.spine",
"fls.nurse.monitoring.mins.spine",
"no.fls.doctor.identification.mins.spine",
"no.fls.doctor.assessment.mins.spine",
"no.fls.doctor.recommendation.mins.spine",
"no.fls.doctor.monitoring.mins.spine",
"fls.doctor.identification.mins.spine",
"fls.doctor.assessment.mins.spine",
"fls.doctor.recommendation.mins.spine",
"fls.doctor.monitoring.mins.spine",
"no.fls.radiographer.identification.mins.spine",
"no.fls.radiographer.assessment.mins.spine",
"no.fls.radiographer.recommendation.mins.spine",
"no.fls.radiographer.monitoring.mins.spine",
"fls.radiographer.identification.mins.spine",
"fls.radiographer.assessment.mins.spine",
"fls.radiographer.recommendation.mins.spine",
"fls.radiographer.monitoring.mins.spine",
"no.fls.allied_health.identification.mins.spine",
"no.fls.allied_health.assessment.mins.spine",
"no.fls.allied_health.recommendation.mins.spine",
"no.fls.allied_health.monitoring.mins.spine",
"fls.allied_health.identification.mins.spine",
"fls.allied_health.assessment.mins.spine",
"fls.allied_health.recommendation.mins.spine",
"fls.allied_health.monitoring.mins.spine",
"no.fls.other.identification.mins.spine",
"no.fls.other.assessment.mins.spine",
"no.fls.other.recommendation.mins.spine",
"no.fls.other.monitoring.mins.spine",
"fls.other.identification.mins.spine",
"fls.other.assessment.mins.spine",
"fls.other.recommendation.mins.spine",
"fls.other.monitoring.mins.spine",

"no.fls.administrator.identification.mins.other",
"no.fls.administrator.assessment.mins.other",
"no.fls.administrator.recommendation.mins.other",
"no.fls.administrator.monitoring.mins.other",
"fls.administrator.identification.mins.other",
"fls.administrator.assessment.mins.other",
"fls.administrator.recommendation.mins.other",
"fls.administrator.monitoring.mins.other",
"fls.fls_coordinator.identification.mins.other",
"fls.fls_coordinator.assessment.mins.other",
"fls.fls_coordinator.recommendation.mins.other",
"fls.fls_coordinator.monitoring.mins.other",
"no.fls.nurse.identification.mins.other",
"no.fls.nurse.assessment.mins.other",
"no.fls.nurse.recommendation.mins.other",
"no.fls.nurse.monitoring.mins.other",
"fls.nurse.identification.mins.other",
"fls.nurse.assessment.mins.other",
"fls.nurse.recommendation.mins.other",
"fls.nurse.monitoring.mins.other",
"no.fls.doctor.identification.mins.other",
"no.fls.doctor.assessment.mins.other",
"no.fls.doctor.recommendation.mins.other",
"no.fls.doctor.monitoring.mins.other",
"fls.doctor.identification.mins.other",
"fls.doctor.assessment.mins.other",
"fls.doctor.recommendation.mins.other",
"fls.doctor.monitoring.mins.other",
"no.fls.radiographer.identification.mins.other",
"no.fls.radiographer.assessment.mins.other",
"no.fls.radiographer.recommendation.mins.other",
"no.fls.radiographer.monitoring.mins.other",
"fls.radiographer.identification.mins.other",
"fls.radiographer.assessment.mins.other",
"fls.radiographer.recommendation.mins.other",
"fls.radiographer.monitoring.mins.other",
"no.fls.allied_health.identification.mins.other",
"no.fls.allied_health.assessment.mins.other",
"no.fls.allied_health.recommendation.mins.other",
"no.fls.allied_health.monitoring.mins.other",
"fls.allied_health.identification.mins.other",
"fls.allied_health.assessment.mins.other",
"fls.allied_health.recommendation.mins.other",
"fls.allied_health.monitoring.mins.other",
"no.fls.other.identification.mins.other",
"no.fls.other.assessment.mins.other",
"no.fls.other.recommendation.mins.other",
"no.fls.other.monitoring.mins.other",
"fls.other.identification.mins.other",
"fls.other.assessment.mins.other",
"fls.other.recommendation.mins.other",
"fls.other.monitoring.mins.other",

"hourly.cost.administrator",
"hourly.cost.fls_coordinator",
"hourly.cost.nurse",
"hourly.cost.doctor",
"hourly.cost.radiographer",
"hourly.cost.allied_health",
"hourly.cost.other",
"prop.lab.test.no.fls.hip",
"prop.lab.test.no.fls.spine",
"prop.lab.test.no.fls.other",
"prop.lab.test.fls.hip",
"prop.lab.test.fls.spine",
"prop.lab.test.fls.other",
"cost.lab.test.hip",
"cost.lab.test.spine",
"cost.lab.test.other",
"prop.dxa.no.fls.hip",
"prop.dxa.no.fls.spine",
"prop.dxa.no.fls.other",
"prop.dxa.fls.hip",
"prop.dxa.fls.spine",
"prop.dxa.fls.other",
"cost.dxa",



"Alendronate.yearly.cost.hip",
"Risedronate.yearly.cost.hip",
"Ibandronate.yearly.cost.hip",
"Raloxifene.yearly.cost.hip",
"Strontium.yearly.cost.hip",
"Denosumab.yearly.cost.hip",
"Zoledronate.yearly.cost.hip",
"Teriparatide.yearly.cost.hip",
"Abaloparatide.yearly.cost.hip",
"Romo.yearly.cost.hip",
"Alendronate.yearly.cost.spine",
"Risedronate.yearly.cost.spine",
"Ibandronate.yearly.cost.spine",
"Raloxifene.yearly.cost.spine",
"Strontium.yearly.cost.spine",
"Denosumab.yearly.cost.spine",
"Zoledronate.yearly.cost.spine",
"Teriparatide.yearly.cost.spine",
"Abaloparatide.yearly.cost.spine",
"Romo.yearly.cost.spine",
"Alendronate.yearly.cost.other",
"Risedronate.yearly.cost.other",
"Ibandronate.yearly.cost.other",
"Raloxifene.yearly.cost.other",
"Strontium.yearly.cost.other",
"Denosumab.yearly.cost.other",
"Zoledronate.yearly.cost.other",
"Teriparatide.yearly.cost.other",
"Abaloparatide.yearly.cost.other",
"Romo.yearly.cost.other",

"fls.database.monthly.maintenance.cost",
"fls.database.installation.cost",
"software.monthly.maintenance.cost",
"software.installation.cost",

"prop.discharged.temp.rehab.hip",
"prop.discharged.home.no.support.hip",
"prop.discharged.home.support.hip",
"prop.discharged.family.home.hip",
"prop.discharged.long.term.care.hip",
"prop.discharged.temp.rehab.spine",
"prop.discharged.home.no.support.spine",
"prop.discharged.home.support.spine",
"prop.discharged.family.home.spine",
"prop.discharged.long.term.care.spine",
"prop.discharged.temp.rehab.other",
"prop.discharged.home.no.support.other",
"prop.discharged.home.support.other",
"prop.discharged.family.home.other",
"prop.discharged.long.term.care.other",

"temp.rehab.los.hip",
"temp.rehab.los.spine",
"temp.rehab.los.other",



"prop.rehab.to.home.no.support.hip",
"prop.rehab.to.home.support.hip",
"prop.rehab.to.family.home.hip",
"prop.rehab.to.long.term.care.hip",
"prop.rehab.to.home.no.support.spine",
"prop.rehab.to.home.support.spine",
"prop.rehab.to.family.home.spine",
"prop.rehab.to.long.term.care.spine",
"prop.rehab.to.home.no.support.other",
"prop.rehab.to.home.support.other",
"prop.rehab.to.family.home.other",
"prop.rehab.to.long.term.care.other",

"clinic.visits.admitted.spine",
"clinic.visits.admitted.hip",
"clinic.visits.admitted.other",
"clinic.visits.not.admitted.spine",
"clinic.visits.not.admitted.hip",
"clinic.visits.not.admitted.other",


"temp.rehab.daily.cost",
"long.term.care.monthly.cost",
"care.home.monthly.cost",
"clinic.visit.cost",

"prop.fx.low.risk",
"prop.fx.intermediate.risk",
"prop.fx.high.risk",

"fx.multiplier.low.risk",
"fx.multiplier.intermediate.risk",
"fx.multiplier.high.risk"

 ),
       function(x) outputOptions(output, x, suspendWhenHidden = FALSE))



eventReactive(input$download_model_inputs, {
  # microsim_pop<-get.microsim_pop()
  # background.tps<-get.background.tps()
  # working.country_data<-get.working.country_data()
  # 
  # user_inputs<-reactiveValuesToList(input)
  # n_microsimulation<-n_microsimulation
  # general.inputs<-general.inputs
  # study_pop_n<-get.study_pop_n()
  # browser()
  # save(list=ls(),
  #      file=here("inputs",
  #                user_inputs$country.name,
  #                "run_model.inputs.RData"),
  #      envir = environment())
})
v <- reactiveValues()

observeEvent(input$download_model_inputs, {
  microsim_pop<-get.microsim_pop()
  background.tps<-get.background.tps()
  working.country_data<-get.working.country_data()
  
  user_inputs<-reactiveValuesToList(input)
  n_microsimulation<-n_microsimulation
  general.inputs<-general.inputs
  study_pop_n<-get.study_pop_n()
  
 # browser()
  if(user_inputs$country.name=="Japan"){
  save(list=ls(),
       file=here("inputs",
                 "Japan",
                 "run_model.inputs.RData"),
       envir = environment())
     v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "Japan",
                                     "run_model.inputs.RData")))}
  
    if(user_inputs$country.name=="Japan (inject only)"){
  save(list=ls(),
       file=here("inputs",
                 "JapanInjectOnly",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "JapanInjectOnly",
                                     "run_model.inputs.RData")))}
  
    if(user_inputs$country.name=="Japan (reduced PFC)"){
  save(list=ls(),
       file=here("inputs",
                 "JapanReducedPfc",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "JapanReducedPfc",
                                     "run_model.inputs.RData")))}
  
    if(user_inputs$country.name=="Netherlands"){
  save(list=ls(),
       file=here("inputs",
                 "Netherlands",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "Netherlands",
                                     "run_model.inputs.RData")))}
  
    if(user_inputs$country.name=="Netherlands (perfect PFC)"){
  save(list=ls(),
       file=here("inputs",
                 "NetherlandsPerfectPfc",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "NetherlandsPerfectPfc",
                                     "run_model.inputs.RData")))
       }
  
    if(user_inputs$country.name=="United Kingdom"){
  save(list=ls(),
       file=here("inputs",
                 "UnitedKingdom",
                 "run_model.inputs.RData"),
       envir = environment())
   v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "UnitedKingdom",
                                     "run_model.inputs.RData")))
    }
  
    if(user_inputs$country.name=="Colombia"){
  save(list=ls(),
       file=here("inputs",
                 "Colombia",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "Colombia",
                                     "run_model.inputs.RData")))}
  
      if(user_inputs$country.name=="France"){
  save(list=ls(),
       file=here("inputs",
                 "France",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "France",
                                     "run_model.inputs.RData")))}
  
      if(user_inputs$country.name=="Mexico"){
  save(list=ls(),
       file=here("inputs",
                 "Mexico",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "Mexico",
                                     "run_model.inputs.RData")))}
  
      if(user_inputs$country.name=="Brazil"){
  save(list=ls(),
       file=here("inputs",
                 "Brazil",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "Brazil",
                                     "run_model.inputs.RData")))}
  
      if(user_inputs$country.name=="Spain"){
  save(list=ls(),
       file=here("inputs",
                 "Spain",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "Spain",
                                     "run_model.inputs.RData")))}
  
  
      if(user_inputs$country.name=="Generic country"){
  save(list=ls(),
       file=here("inputs",
                 "GenericCountry",
                 "run_model.inputs.RData"),
       envir = environment())
       v$message <-paste0("Model inputs last saved: ", 
                     file.mtime(here("inputs",
                                     "GenericCountry",
                                     "run_model.inputs.RData")))}
  
  

})



output$download_model_inputs_text <- renderText({
  if (is.null(v$message)){
  paste0("Model inputs last saved: ", 
         file.mtime(here("inputs",
                         input$country.name,
                         "run_model.inputs.RData")))} else {
                           v$message      
                         }
  
  
})

get.m.TR<-
  reactive({ 

microsim_pop<-get.microsim_pop() 
background.tps<-get.background.tps()
working.country_data<-get.working.country_data()

#for saving
#browser()
# user_inputs<-reactiveValuesToList(input)
# n_microsimulation<-n_microsimulation
# general.inputs<-general.inputs
# study_pop_n<-get.study_pop_n()
# save(list=ls(),
#        file=here("inputs", 
#                   user_inputs$country.name,
#                  "run_model.inputs.RData"),
#       envir = environment())





##### MODEL #######
### SET UP ####
# markov trace  --------
number.cycles<-60
# 1 month cycles, 5 years 

# set up:
# list of dataframes
# each item in the list is a dataframe
# each dataframe corresponds to an individual under a treatment option
# e.g. one dataframe for first individual with no FLS,
# another with the first individual with FLS,
# and so on

 # browser()
 m.TR <- rbind(
    data.frame(cycle= rep(1:(number.cycles+1), 
                          times=nrow(microsim_pop)),
               month=seq(0,60,1),
               year= c(0, rep(1,12), rep(2,12),rep(3,12), rep(4,12), rep(5,12)),
               id= rep(1:nrow(microsim_pop), each=(number.cycles+1)),# individual id 
               intervention="no FLS",
               # states
               "s_hf"=NA,  # hip fracture
               "s_sf"=NA,  # spine fracture
               "s_of"=NA,  # other fracture
               "s_ff"=NA,  # fracture free
               "s_d"=NA,   # death 
               # currently in a fracture state?
               c_af=NA, 
               # history tracker
               h_hf=NA,    # history of hip fracture (how many experienced)
               h_sf=NA,    # history of spine fracture (how many experienced)
               h_of=NA,    # history of other fracture (how many experienced)
               h_af=NA,    # history of any fracture (how many experienced),
               recent.fx=NA, #most recent fx 
               identified=NA,
               tr.onset=NA,
               treat=NA,
               medication=NA,
               monitored=NA,
               adhering=NA,
               time_med=NA,
               lag.passed=NA, # whether time on treatment is over lag
               apply.rr=NA, # was relative risk applied
               hospitalised=NA, # currently hospitalised
               procedure=NA, # procedure in hospital
               temp.rehab=NA,
               location=NA,
               lab.test=NA,
               dxa=NA
               
    ), 
    data.frame(cycle= rep(1:(number.cycles+1), times=nrow(microsim_pop)),
               month=seq(0,60,1),
               year= c(0, rep(1,12), rep(2,12),rep(3,12), rep(4,12), rep(5,12)),
               id= rep(1:nrow(microsim_pop), each=(number.cycles+1)), # individual id 
               intervention="FLS",
               "s_hf"=NA,  # hip fracture
               "s_sf"=NA,  # spine fracture
               "s_of"=NA,  # other fracture
               "s_ff"=NA,  # fracture free
               "s_d"=NA, # death
               # currently in a fracture state?
               c_af=NA, 
               # history tracker
               h_hf=NA,    # history of hip fracture (how many experienced)
               h_sf=NA,    # history of spine fracture (how many experienced)
               h_of=NA,    # history of other fracture (how many experienced)
               h_af=NA,    # history of any fracture  (how many experienced)
               recent.fx=NA, #most recent fx 
               identified=NA,
               tr.onset=NA,
               treat=NA,
               medication=NA,
               monitored=NA,
               adhering=NA,
               time_med=NA,
               lag.passed=NA, # whether time on treatment is over lag
               apply.rr=NA, # was relative risk applied
               hospitalised=NA, # currently hospitalised
               procedure=NA, # procedure in hospital
               temp.rehab=NA,
               location=NA,
               lab.test=NA,
               dxa=NA
    ))
  # so we have each individual twice
  # once for no FLS
  # another without FLS
  
# add individual's characteristics -----
  m.TR<- m.TR %>% 
    left_join(microsim_pop, 
              by="id")
 # drop if missing index fx (study pop=0 ) 
m.TR<- m.TR %>% 
   filter(!is.na(index_fx))
  
# add working age
  m.TR$working.age<-m.TR$age+m.TR$year
  
  
  
# starting state --------
  m.TR[which(m.TR$month==0),] <- m.TR[which(m.TR$month==0),]  %>% 
    mutate(s_hf=ifelse(index_fx=="hip" , 1, 0)) %>% 
    mutate(s_sf=ifelse(index_fx=="spine", 1, 0)) %>% 
    mutate(s_of=ifelse(index_fx=="other", 1, 0)) %>% 
    mutate(s_ff=0,
           s_d=0)
  
# start in a fracture state
   m.TR[which(m.TR$month==0),] <- m.TR[which(m.TR$month==0),]  %>% 
     mutate(c_af=1)
  
# no history yet 
  m.TR[which(m.TR$month==0),] <- m.TR[which(m.TR$month==0),]  %>% 
    mutate(h_hf= 0) %>% 
    mutate(h_sf=0)  %>% 
    mutate(h_of=0)  %>%
    mutate(h_af=0) # i.e. no history of fracture (will have from next period....)
  
  
# recent.fx
m.TR[which(m.TR$month==0),] <-  m.TR[which(m.TR$month==0),]  %>% 
    mutate(recent.fx= ifelse(index_fx=="spine" , 1, 
                      ifelse(index_fx=="hip" , 2,  
                      ifelse(index_fx=="other" , 3, NA))))
# identified- no
m.TR[which(m.TR$month==0),] <-  m.TR[which(m.TR$month==0),]  %>% 
  mutate(identified=1,
         tr.onset=1,
         treat=1,
         medication=0,
         monitored=1,
         adhering=1,
         lag.passed=0,
         time_med=0)

  

# history -----
m.TR$t_since_sf<- NA     
m.TR$t_since_hf<- NA     
m.TR$t_since_of<- NA   

m.TR$sf.tp<- NA 
m.TR$hf.tp<- NA 
m.TR$of.tp<- NA 
m.TR$of.tp<- NA
m.TR$tp.death<- NA

# as list  ---------
m.TR<-m.TR %>% 
  mutate(intervention=ifelse(intervention=="no FLS",
                             1,2)) %>% 
      mutate(index_fx=ifelse(index_fx=="spine",
                             1,
                      ifelse(index_fx=="hip",
                             2,       
                             3))) %>% 
     mutate(sex=ifelse(sex=="male",
                             1,2)) 

m.TR <-  split(m.TR,
                 m.TR[,c('id','intervention')])


## rr ---------
rr_alendronate_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_alendronate_spine") %>%
    select(Value))
rr_risedronate_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_risedronate_spine") %>%
    select(Value))
rr_strontium_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_strontium_spine") %>%
    select(Value))
rr_ibandronate_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_ibandronate_spine") %>%
    select(Value))
rr_raloxifene_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_raloxifene_spine") %>%
    select(Value))
rr_denosumab_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_denosumab_spine") %>%
    select(Value))
rr_zoledronate_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_zoledronate_spine") %>%
    select(Value))
rr_teriparatide_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_teriparatide_spine") %>%
    select(Value))
rr_abaloparatide_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_abaloparatide_spine") %>%
    select(Value))
rr_romo_spine<-as.numeric(working.country_data %>%
    filter(name=="rr_romo_spine") %>%
    select(Value))

rr_alendronate_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_alendronate_hip") %>%
    select(Value))
rr_risedronate_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_risedronate_hip") %>%
    select(Value))
rr_strontium_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_strontium_hip") %>%
    select(Value))
rr_ibandronate_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_ibandronate_hip") %>%
    select(Value))
rr_raloxifene_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_raloxifene_hip") %>%
    select(Value))
rr_denosumab_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_denosumab_hip") %>%
    select(Value))
rr_zoledronate_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_zoledronate_hip") %>%
    select(Value))
rr_teriparatide_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_teriparatide_hip") %>%
    select(Value))
rr_abaloparatide_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_abaloparatide_hip") %>%
    select(Value))
rr_romo_hip<-as.numeric(working.country_data %>%
    filter(name=="rr_romo_hip") %>%
    select(Value))

rr_alendronate_other<-as.numeric(working.country_data %>%
    filter(name=="rr_alendronate_other") %>%
    select(Value))
rr_risedronate_other<-as.numeric(working.country_data %>%
    filter(name=="rr_risedronate_other") %>%
    select(Value))
rr_strontium_other<-as.numeric(working.country_data %>%
    filter(name=="rr_strontium_other") %>%
    select(Value))
rr_ibandronate_other<-as.numeric(working.country_data %>%
    filter(name=="rr_ibandronate_other") %>%
    select(Value))
rr_raloxifene_other<-as.numeric(working.country_data %>%
    filter(name=="rr_raloxifene_other") %>%
    select(Value))
rr_denosumab_other<-as.numeric(working.country_data %>%
    filter(name=="rr_denosumab_other") %>%
    select(Value))
rr_zoledronate_other<-as.numeric(working.country_data %>%
    filter(name=="rr_zoledronate_other") %>%
    select(Value))
rr_teriparatide_other<-as.numeric(working.country_data %>%
    filter(name=="rr_teriparatide_other") %>%
    select(Value))
rr_abaloparatide_other<-as.numeric(working.country_data %>%
    filter(name=="rr_abaloparatide_other") %>%
    select(Value))
rr_romo_other<-as.numeric(working.country_data %>%
    filter(name=="rr_romo_other") %>%
    select(Value))


# run model
# add info on costs for first cycle -----
#browser()
rmultinorm<-get.rmultinorm(input)

m.TR<-  lapply(m.TR,
   function(df, t=1){

     df<-as.matrix(df)



# FOR costing -----
# get hospitalised ------
df<-m.fun.hospitalised(df=df, t=t, rmultinorm=rmultinorm,  input=input)
# get procedure ------
df<-m.fun.procedure(df=df, t=t, rmultinorm=rmultinorm,  input=input)
# get temp rehab ------
#browser()
df<-m.fun.temp.rehab(df=df, t=t, rmultinorm=rmultinorm,  input=input)
# get location -----
df<-m.fun.location(df=df, t=t, rmultinorm=rmultinorm,  input=input)
# get lab test -----
df<-m.fun.lab.test(df=df, t=t, rmultinorm=rmultinorm,  input=input)
# get dxa -----
df<-m.fun.dxa(df=df, t=t, rmultinorm=rmultinorm,  input=input)


df

})

m.TR
# run for rest of the cycles -----
    for (i in 2:61) { 
      
      if(n_microsimulation>100){
        id <- showNotification(paste0("Getting transitions for month ", i-1, " of 60"), 
                               duration = 3, closeButton = TRUE) } 
      
      if(n_microsimulation<=100){
        if(i %in% seq(5,61,4)){
          id <- showNotification(paste0("Getting transitions for month ", i-1, " of 60"), 
                                 duration = 3, closeButton = TRUE) }}
      
      working.t<-i 
      
      
      
      #browser()
      rmultinorm<-get.rmultinorm(input)
      # run -----
      
      # set up
      m.TR<-
        lapply(m.TR,
               # parLapply(cl, m.TR,
               function(df, t={{working.t}}){
                 
                 df<-as.matrix(df)
                 
                 # time since fx 
                 df<-m.fun.time.since.fx(df=df, t=t)
                 
                 # most recent fx
                 df[t,"recent.fx"] <-which.min(c(df[t,"t_since_sf"],
                                                 df[t,"t_since_hf"],
                                                 df[t,"t_since_of"])) 
                 
                 # transition probabilities 
                 df<-m.fun.tps(df=df,t=t, background.tps=background.tps)
                 
                 # max two hip fx
                 df<-m.fun.two.fx(df=df, t=t)
                 
                 # identified
                 df<-m.fun.identified(df=df, t=t, rmultinorm=rmultinorm)
                 
                 # tr onset
                 df<-m.fun.tr.onset(df=df, t=t, input=input)
                 
                 
                 # treat
                 df<-m.fun.treat(df=df, t=t, rmultinorm=rmultinorm)
                 #browser()
                 # medication 
                 df<-m.fun.assign.treatment(df=df, t=t, rmultinorm=rmultinorm)
                 
                 # switching medication
                 df<-m.fun.switch.treatment(df=df, t=t, rmultinorm=rmultinorm)
                 
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
                     if(df[t,"time_med"]>(lag_alendronate_spine)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_hf"])) {
                     if(df[t,"time_med"]>(lag_alendronate_hip)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_of"])) {
                     if(df[t,"time_med"]>(lag_alendronate_other)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                 }
                 
                 if(df[t,"medication"] ==2) {#risedronate
                   
                   if(!is.na(df[t,"t_since_sf"])) {
                     if(df[t,"time_med"]>(lag_risedronate_spine)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_hf"])) {
                     if(df[t,"time_med"]>(lag_risedronate_hip)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_of"])) {
                     if(df[t,"time_med"]>(lag_risedronate_other)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                 }
                 
                 if(df[t,"medication"] ==3) {#strontium
                   
                   if(!is.na(df[t,"t_since_sf"])) {
                     if(df[t,"time_med"]>(lag_strontium_spine)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_hf"])) {
                     if(df[t,"time_med"]>(lag_strontium_hip)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_of"])) {
                     if(df[t,"time_med"]>(lag_strontium_other)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                 }
                 
                 if(df[t,"medication"] ==4) {#ibandronate
                   
                   if(!is.na(df[t,"t_since_sf"])) {
                     if(df[t,"time_med"]>(lag_ibandronate_spine)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_hf"])) {
                     if(df[t,"time_med"]>(lag_ibandronate_hip)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_of"])) {
                     if(df[t,"time_med"]>(lag_ibandronate_other)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                 }
                 
                 if(df[t,"medication"] ==5) {#denosumab
                   
                   if(!is.na(df[t,"t_since_sf"])) {
                     if(df[t,"time_med"]>(lag_denosumab_spine)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_hf"])) {
                     if(df[t,"time_med"]>(lag_denosumab_hip)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_of"])) {
                     if(df[t,"time_med"]>(lag_denosumab_other)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                 }
                 
                 if(df[t,"medication"] ==6) {#zoledronate
                   
                   if(!is.na(df[t,"t_since_sf"])) {
                     if(df[t,"time_med"]>(lag_zoledronate_spine)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_hf"])) {
                     if(df[t,"time_med"]>(lag_zoledronate_hip)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_of"])) {
                     if(df[t,"time_med"]>(lag_zoledronate_other)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                 }
                 
                 if(df[t,"medication"] ==7) {#teriparatide
                   
                   if(!is.na(df[t,"t_since_sf"])) {
                     if(df[t,"time_med"]>(lag_teriparatide_spine)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_hf"])) {
                     if(df[t,"time_med"]>(lag_teriparatide_hip)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_of"])) {
                     if(df[t,"time_med"]>(lag_teriparatide_other)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                 }
                 
                 if(df[t,"medication"] ==8) {#abaloparatide
                   
                   if(!is.na(df[t,"t_since_sf"])) {
                     if(df[t,"time_med"]>(lag_abaloparatide_spine)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_hf"])) {
                     if(df[t,"time_med"]>(lag_abaloparatide_hip)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_of"])) {
                     if(df[t,"time_med"]>(lag_abaloparatide_other)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                 }
                 
                 if(df[t,"medication"] ==9) {#romo
                   
                   if(!is.na(df[t,"t_since_sf"])) {
                     if(df[t,"time_med"]>(lag_romo_spine)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_hf"])) {
                     if(df[t,"time_med"]>(lag_romo_hip)){
                       df[t,"lag.passed"]<-1
                     }}
                   
                   if(!is.na(df[t,"t_since_of"])) {
                     if(df[t,"time_med"]>(lag_romo_other)){
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
                 
                 if(df[t,"time_med"]==1){
                   df<-m.fun.adherrence.t1(df=df, t=t, rmultinorm=rmultinorm)
                 } else if(df[t,"time_med"]==4){
                   df<-m.fun.adherrence.t4(df=df, t=t, rmultinorm=rmultinorm)
                 } else if(df[t,"time_med"]==12){
                   df<-m.fun.adherrence.t12(df=df, t=t, rmultinorm=rmultinorm)
                 } else if(df[t,"time_med"]==24|
                           df[t,"time_med"]==36|
                           df[t,"time_med"]==48){
                   df<-m.fun.adherrence.t24_36_48(df=df, t=t, rmultinorm=rmultinorm)
                 } else {# if not time 1, 4, 12 same as in previous period  
                   df[t,"adhering"]<-df[t-1,"adhering"]
                 }
                 
                 
                 # apply rr of drug ######
                 # on given drug, identified, tr onset, tr are all 2 
                 # adhering 
                 
                 
                 # only if on drug above lag
                 
                 if(df[t,"adhering"]==2){
                   if(df[t,"lag.passed"]==1){
                     
                     if(df[t,"medication"]==1 ){ #alendronate
                       df[t,"sf.tp"] <-df[t,"sf.tp"]*rr_alendronate_spine
                       df[t,"hf.tp"] <-df[t,"hf.tp"]*rr_alendronate_hip
                       df[t,"of.tp"] <-df[t,"of.tp"]*rr_alendronate_other
                       
                       df[t,"apply.rr"] <-1
                       
                     }
                     if(df[t,"medication"]==2 ){ #risedronate
                       df[t,"sf.tp"] <-df[t,"sf.tp"]*rr_risedronate_spine
                       df[t,"hf.tp"] <-df[t,"hf.tp"]*rr_risedronate_hip
                       df[t,"of.tp"] <-df[t,"of.tp"]*rr_risedronate_other
                       
                       df[t,"apply.rr"] <-1
                     }
                     if(df[t,"medication"]==3 ){ #strontium
                       df[t,"sf.tp"] <-df[t,"sf.tp"]*rr_strontium_spine
                       df[t,"hf.tp"] <-df[t,"hf.tp"]*rr_strontium_hip
                       df[t,"of.tp"] <-df[t,"of.tp"]*rr_strontium_other
                       
                       df[t,"apply.rr"] <-1
                     }
                     if(df[t,"medication"]==4 ){ #ibandronate
                       df[t,"sf.tp"] <-df[t,"sf.tp"]*rr_ibandronate_spine
                       df[t,"hf.tp"] <-df[t,"hf.tp"]*rr_ibandronate_hip
                       df[t,"of.tp"] <-df[t,"of.tp"]*rr_ibandronate_other
                       
                       df[t,"apply.rr"] <-1
                     }
                     if(df[t,"medication"]==5 ){ #denosumab
                       df[t,"sf.tp"] <-df[t,"sf.tp"]*rr_denosumab_spine
                       df[t,"hf.tp"] <-df[t,"hf.tp"]*rr_denosumab_hip
                       df[t,"of.tp"] <-df[t,"of.tp"]*rr_denosumab_other
                       
                       df[t,"apply.rr"] <-1
                     }
                     if(df[t,"medication"]==6 ){ #zoledronate
                       df[t,"sf.tp"] <-df[t,"sf.tp"]*rr_zoledronate_spine
                       df[t,"hf.tp"] <-df[t,"hf.tp"]*rr_zoledronate_hip
                       df[t,"of.tp"] <-df[t,"of.tp"]*rr_zoledronate_other
                       
                       df[t,"apply.rr"] <-1
                     }
                     if(df[t,"medication"]==7 ){ #teriparatide
                       df[t,"sf.tp"] <-df[t,"sf.tp"]*rr_teriparatide_spine
                       df[t,"hf.tp"] <-df[t,"hf.tp"]*rr_teriparatide_hip
                       df[t,"of.tp"] <-df[t,"of.tp"]*rr_teriparatide_other
                       
                       df[t,"apply.rr"] <-1
                     }
                     if(df[t,"medication"]==8 ){ #abaloparatide
                       df[t,"sf.tp"] <-df[t,"sf.tp"]*rr_abaloparatide_spine
                       df[t,"hf.tp"] <-df[t,"hf.tp"]*rr_abaloparatide_hip
                       df[t,"of.tp"] <-df[t,"of.tp"]*rr_abaloparatide_other
                       
                       df[t,"apply.rr"] <-1
                     }
                     if(df[t,"medication"]==9 ){ #romo
                       df[t,"sf.tp"] <-df[t,"sf.tp"]*rr_romo_spine
                       df[t,"hf.tp"] <-df[t,"hf.tp"]*rr_romo_hip
                       df[t,"of.tp"] <-df[t,"of.tp"]*rr_romo_other
                       
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
                   working.transition<-  as.numeric(
                     rMultinom(t(
                       as.matrix(c(as.numeric(df[t,"hf.tp"]),
                                   as.numeric(df[t,"of.tp"]),
                                   as.numeric(df[t,"sf.tp"]),
                                   as.numeric( df[t,"tp.death"]),
                                   1-    as.numeric(df[t,"hf.tp"])-
                                     as.numeric(df[t,"of.tp"])-
                                     as.numeric(df[t,"sf.tp"])-
                                     as.numeric(df[t,"tp.death"])   ))),1))
                   
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
                 
                 
                 
                 
                 # FOR costing -----
                 #browser()
                 # get hospitalised ------
                 df<-m.fun.hospitalised(df=df, t=t, rmultinorm=rmultinorm,  input=input)
                 # get procedure ------
                 df<-m.fun.procedure(df=df, t=t, rmultinorm=rmultinorm,  input=input)
                 # get temp rehab ------
                 df<-m.fun.temp.rehab(df=df, t=t, rmultinorm=rmultinorm,  input=input)
                 # # get location -----
                 #browser
                 df<-m.fun.location(df=df, t=t, rmultinorm=rmultinorm,  input=input)
                 # get lab test -----
                 df<-m.fun.lab.test(df=df, t=t, rmultinorm=rmultinorm,  input=input)
                 # get dxa -----
                 df<-m.fun.dxa(df=df, t=t, rmultinorm=rmultinorm,  input=input)
                 
                 
                 df
                 
               })
      
    }
    
    
    m.TR<-plyr::ldply(m.TR, data.frame, .id=NULL)
    m.TR
# working output -----    
m.TR<-m.TR %>%
  mutate(intervention=ifelse(intervention==1, "no FLS", "FLS")) %>%
   mutate(identified=ifelse(identified==1,"No","Yes")) %>%
   mutate(apply.rr=ifelse(is.na(apply.rr),"No","Yes")) %>%
#   mutate(tr.onset=ifelse(tr.onset==1,"No","Yes")) %>%
  mutate(treat=ifelse(treat==1,"No","Yes")) %>%
    mutate(adhering=ifelse(adhering==1,"No","Yes")) %>%
      mutate(index_fx=ifelse(index_fx==1, "spine",
                      ifelse(index_fx==2, "hip", "other"))) %>%
   mutate(recent.fx=ifelse(recent.fx==1, "spine",
                     ifelse(recent.fx==2, "hip",
                     ifelse(recent.fx==3, "other",NA)))) %>% 
     mutate(sex=ifelse(sex==1, "male", "female")) %>%
   mutate(sex=factor(sex,   
         levels=c("male", "female"))) %>% 
   mutate(medication=
           ifelse(medication== 0,"none",
           ifelse(medication==1,"alendronate",
           ifelse(medication==2,"risedronate",
           ifelse(medication==3,"strontium",
           ifelse(medication==4,"ibandronate",
           ifelse(medication==5,"denosumab",
           ifelse(medication==6,"zoledronate",
           ifelse(medication==7,"teriparatide",
           ifelse(medication==8,"abaloparatide",
           ifelse(medication== 9,"romo",
                   NA))))))))))) %>% 
  mutate(hospitalised=
           ifelse(hospitalised==1, "no", 
           ifelse(hospitalised==2,"yes", NA))) %>% 
  mutate(procedure=
          ifelse(procedure== 1,"none",
           ifelse(procedure==2,"hip surgery",
           ifelse(procedure==3,"other surgery",
           ifelse(procedure==4,"kyphoplasty",
           ifelse(procedure==5,"vertebroplasty", NA
           )))))) %>% 
  mutate(temp.rehab=
           ifelse(temp.rehab==1, "no", 
           ifelse(temp.rehab==2,"yes", NA))) %>% 
  mutate(location=
           ifelse(location==1, "home, no support",
          ifelse(location==2, "home, support",
          ifelse(location==3, "family home",
          ifelse(location==4, "long term care", NA)))))
           



# get costs -----
 #browser()
m.TR<-m.TR %>% 
  mutate(procedure.cost=ifelse(procedure=="none",0,
        ifelse(procedure=="hip surgery", input$cost.hip.surg,
ifelse(procedure=="other surgery", input$cost.other.surg,
ifelse(procedure=="kyphoplasty", input$cost.spine.kyphoplasty,
ifelse(procedure=="vertebroplasty", input$cost.spine.vertebroplasty,
       0)))))) 

# hospital length of stay ----
# if hospitalised, depending on index fracture and procedure
m.TR<-m.TR %>% 
  mutate(hosp.los=
   ifelse(hospitalised=="no", 0,
   ifelse(hospitalised=="yes" &
          procedure=="none"  &
          s_hf==1, input$hospital.los.hip.no.surg,
   ifelse(hospitalised=="yes" &
          procedure=="hip surgery", input$hospital.los.hip.surg,
   ifelse(hospitalised=="yes" &
          procedure=="none"  &
          s_sf==1, input$hospital.los.spine.no.surg, 
   ifelse(hospitalised=="yes" &
          procedure=="kyphoplasty", input$hospital.los.spine.kyphoplasty,
   ifelse(hospitalised=="yes" &
          procedure=="vertebroplasty", input$hospital.los.spine.vertebroplasty,    
   ifelse(hospitalised=="yes" &
          procedure=="none"  &
          s_of==1, input$hospital.los.other.no.surg,    
   ifelse(hospitalised=="yes" &
          procedure=="other surgery", input$hospital.los.other.surg,        
           NA)))))))))

           
# hospital cost ----
# for those hospitalised, given procedure and length of stay
m.TR<-m.TR %>% 
  mutate(hosp.cost=
   ifelse(hospitalised=="no", 0,
   ifelse(hospitalised=="yes", 
       (hosp.los*input$cost.hosp.per.day)+
         procedure.cost, NA)))


# community cost ----
#browser()

# spine only
# dependent on procedure
m.TR<-m.TR %>% 
  mutate(comm.cost=
   ifelse(hospitalised=="no" &  
          s_sf==1,  
        (input$visits.comm.consults.spine* input$cost.spine.community.care)+ 
         procedure.cost, 0))


# clinic cost ----
# for only those not hospitalised(?),
# so for only hip and other
# with cost of one(?) visit
m.TR<-m.TR %>% 
  mutate(clinic.cost=
   ifelse(hospitalised=="no" &  
          s_hf==1,  
        input$cost.hosp.clinic.visit ,
  ifelse(hospitalised=="no" &  
          s_of==1, 
        input$cost.hosp.clinic.visit ,
         0)))  

# temp rehab los and cost
m.TR<-m.TR %>% 
  mutate(temp.rehab.los=
     ifelse(temp.rehab=="no", 0,
     ifelse(temp.rehab=="yes"&
            s_hf==1,   input$temp.rehab.los.hip, 
      ifelse(temp.rehab=="yes"&
            s_sf==1,   input$temp.rehab.los.spine, 
      ifelse(temp.rehab=="yes"&
            s_of==1,   input$temp.rehab.los.other, 
            NA))))) %>% 
  mutate(temp.rehab.cost=temp.rehab.los*input$temp.rehab.daily.cost)
            
# location cost -----
m.TR<-m.TR %>% 
  mutate(location.cost=
           ifelse(location=="home, support",
                  input$care.home.monthly.cost,
           ifelse(location=="long term care",
                  input$long.term.care.monthly.cost,
                  0  )))        

# discharge clinic cost -----
m.TR<-m.TR %>% 
  mutate(discharge.clinic.cost=
    ifelse(
      hospitalised=="no" &  s_hf==1, 
      input$clinic.visits.not.admitted.hip*input$clinic.visit.cost,
       ifelse(
      hospitalised=="no" &  s_sf==1, 
      input$clinic.visits.not.admitted.spine*input$clinic.visit.cost,   
      ifelse(
      hospitalised=="no" &  s_of==1, 
      input$clinic.visits.not.admitted.other*input$clinic.visit.cost,   

   ifelse(
      hospitalised=="yes" &  s_hf==1, 
      input$clinic.visits.admitted.hip*input$clinic.visit.cost,
       ifelse(
      hospitalised=="yes" &  s_sf==1, 
      input$clinic.visits.admitted.spine*input$clinic.visit.cost,   
      ifelse(
      hospitalised=="yes" &  s_of==1, 
      input$clinic.visits.admitted.other*input$clinic.visit.cost,         
      0)))))))
      


# medication cost -----
# only occurred if adhering
# from yearly to monthly cost
#based on most recent fx(?)

m.TR<-m.TR %>% 
  mutate(medication.cost=
           # hip
    ifelse(
      medication=="alendronate" &  adhering=="Yes" & recent.fx=="hip",
    input$Alendronate.yearly.cost.hip/12,
    ifelse(
      medication=="risedronate" &  adhering=="Yes" & recent.fx=="hip",
    input$Risedronate.yearly.cost.hip/12,
    ifelse(
      medication=="ibandronate" &  adhering=="Yes" & recent.fx=="hip",
    input$Ibandronate.yearly.cost.hip/12,
    ifelse(
      medication=="strontium" &  adhering=="Yes" & recent.fx=="hip",
    input$Strontium.yearly.cost.hip/12,
    ifelse(
      medication=="denosumab" &  adhering=="Yes" & recent.fx=="hip",
    input$Denosumab.yearly.cost.hip/12,
    ifelse(
      medication=="zoledronate" &  adhering=="Yes" & recent.fx=="hip",
    input$Zoledronate.yearly.cost.hip/12,
    ifelse(
      medication=="teriparatide" &  adhering=="Yes" & recent.fx=="hip",
    input$Teriparatide.yearly.cost.hip/12,
    ifelse(
      medication=="abaloparatide" &  adhering=="Yes" & recent.fx=="hip",
    input$Abaloparatide.yearly.cost.hip/12,
    ifelse(
      medication=="romo" &  adhering=="Yes" & recent.fx=="hip",
    input$Romo.yearly.cost.hip/12,
               # spine
    ifelse(
      medication=="alendronate" &  adhering=="Yes" & recent.fx=="spine",
    input$Alendronate.yearly.cost.spine/12,
    ifelse(
      medication=="risedronate" &  adhering=="Yes" & recent.fx=="spine",
    input$Risedronate.yearly.cost.spine/12,
    ifelse(
      medication=="ibandronate" &  adhering=="Yes" & recent.fx=="spine",
    input$Ibandronate.yearly.cost.spine/12,
    ifelse(
      medication=="strontium" &  adhering=="Yes" & recent.fx=="spine",
    input$Strontium.yearly.cost.spine/12,
    ifelse(
      medication=="denosumab" &  adhering=="Yes" & recent.fx=="spine",
    input$Denosumab.yearly.cost.spine/12,
    ifelse(
      medication=="zoledronate" &  adhering=="Yes" & recent.fx=="spine",
    input$Zoledronate.yearly.cost.spine/12,
    ifelse(
      medication=="teriparatide" &  adhering=="Yes" & recent.fx=="spine",
    input$Teriparatide.yearly.cost.spine/12,
    ifelse(
      medication=="abaloparatide" &  adhering=="Yes" & recent.fx=="spine",
    input$Abaloparatide.yearly.cost.spine/12,
    ifelse(
      medication=="romo" &  adhering=="Yes" & recent.fx=="spine",
    input$Romo.yearly.cost.spine/12,
              # other
    ifelse(
      medication=="alendronate" &  adhering=="Yes" & recent.fx=="other",
    input$Alendronate.yearly.cost.other/12,
    ifelse(
      medication=="risedronate" &  adhering=="Yes" & recent.fx=="other",
    input$Risedronate.yearly.cost.other/12,
    ifelse(
      medication=="ibandronate" &  adhering=="Yes" & recent.fx=="other",
    input$Ibandronate.yearly.cost.other/12,
    ifelse(
      medication=="strontium" &  adhering=="Yes" & recent.fx=="other",
    input$Strontium.yearly.cost.other/12,
    ifelse(
      medication=="denosumab" &  adhering=="Yes" & recent.fx=="other",
    input$Denosumab.yearly.cost.other/12,
    ifelse(
      medication=="zoledronate" &  adhering=="Yes" & recent.fx=="other",
    input$Zoledronate.yearly.cost.other/12,
    ifelse(
      medication=="teriparatide" &  adhering=="Yes" & recent.fx=="other",
    input$Teriparatide.yearly.cost.other/12,
    ifelse(
      medication=="abaloparatide" &  adhering=="Yes" & recent.fx=="other",
    input$Abaloparatide.yearly.cost.other/12,
    ifelse(
      medication=="romo" &  adhering=="Yes" & recent.fx=="other",
    input$Romo.yearly.cost.other/12,
   0 ))))))))))))))))))))))))))))



# fx prevention costs ------
# only applied when an individual is identified

# staff cost
# no.fls 

no.fls.hip.staff.cost<-
(
  ((input$no.fls.administrator.identification.mins.hip +
input$no.fls.administrator.assessment.mins.hip +
input$no.fls.administrator.recommendation.mins.hip +
input$no.fls.administrator.monitoring.mins.hip)*
 input$hourly.cost.administrator)+
((input$no.fls.nurse.identification.mins.hip +
input$no.fls.nurse.assessment.mins.hip +
input$no.fls.nurse.recommendation.mins.hip +
input$no.fls.nurse.monitoring.mins.hip )*
 input$hourly.cost.nurse)+
((input$no.fls.doctor.identification.mins.hip +
input$no.fls.doctor.assessment.mins.hip +
input$no.fls.doctor.recommendation.mins.hip +
input$no.fls.doctor.monitoring.mins.hip )*
 input$hourly.cost.doctor)+
((input$no.fls.radiographer.identification.mins.hip +
input$no.fls.radiographer.assessment.mins.hip +
input$no.fls.radiographer.recommendation.mins.hip +
input$no.fls.radiographer.monitoring.mins.hip )*
 input$hourly.cost.radiographer)+
  ((input$no.fls.allied_health.identification.mins.hip +
input$no.fls.allied_health.assessment.mins.hip +
input$no.fls.allied_health.recommendation.mins.hip +
input$no.fls.allied_health.monitoring.mins.hip )*
 input$hourly.cost.allied_health) +
  ((input$no.fls.administrator.identification.mins.hip +
input$no.fls.administrator.assessment.mins.hip +
input$no.fls.administrator.recommendation.mins.hip +
input$no.fls.administrator.monitoring.mins.hip)*
 input$hourly.cost.administrator)# +
#   ((input$no.fls.other.identification.mins.hip +
# input$no.fls.other.assessment.mins.hip +
# input$no.fls.other.recommendation.mins.hip +
# input$no.fls.other.monitoring.mins.hip )*
#  input$hourly.cost.other)
) 

# if all zero
no.fls.hip.staff.cost<-ifelse(length(no.fls.hip.staff.cost)==0,
                              0, no.fls.hip.staff.cost)

no.fls.spine.staff.cost<-
(((input$no.fls.administrator.identification.mins.spine +
input$no.fls.administrator.assessment.mins.spine +
input$no.fls.administrator.recommendation.mins.spine +
input$no.fls.administrator.monitoring.mins.spine)*
 input$hourly.cost.administrator)+
((input$no.fls.nurse.identification.mins.spine +
input$no.fls.nurse.assessment.mins.spine +
input$no.fls.nurse.recommendation.mins.spine +
input$no.fls.nurse.monitoring.mins.spine )*
 input$hourly.cost.nurse)+
((input$no.fls.doctor.identification.mins.spine +
input$no.fls.doctor.assessment.mins.spine +
input$no.fls.doctor.recommendation.mins.spine +
input$no.fls.doctor.monitoring.mins.spine )*
 input$hourly.cost.doctor)+
((input$no.fls.radiographer.identification.mins.spine +
input$no.fls.radiographer.assessment.mins.spine +
input$no.fls.radiographer.recommendation.mins.spine +
input$no.fls.radiographer.monitoring.mins.spine )*
 input$hourly.cost.radiographer)+
  ((input$no.fls.allied_health.identification.mins.spine +
input$no.fls.allied_health.assessment.mins.spine +
input$no.fls.allied_health.recommendation.mins.spine +
input$no.fls.allied_health.monitoring.mins.spine )*
 input$hourly.cost.allied_health)+
  ((input$no.fls.administrator.identification.mins.spine +
input$no.fls.administrator.assessment.mins.spine +
input$no.fls.administrator.recommendation.mins.spine +
input$no.fls.administrator.monitoring.mins.spine)*
 input$hourly.cost.administrator)#+
#   ((input$no.fls.other.identification.mins.spine +
# input$no.fls.other.assessment.mins.spine +
# input$no.fls.other.recommendation.mins.spine +
# input$no.fls.other.monitoring.mins.spine )*
#  input$hourly.cost.other)
)
# if all zero
no.fls.spine.staff.cost<-ifelse(length(no.fls.spine.staff.cost)==0,
                              0, no.fls.spine.staff.cost)
no.fls.other.staff.cost<-
(((input$no.fls.administrator.identification.mins.other +
input$no.fls.administrator.assessment.mins.other +
input$no.fls.administrator.recommendation.mins.other +
input$no.fls.administrator.monitoring.mins.other)*
 input$hourly.cost.administrator)+
((input$no.fls.nurse.identification.mins.other +
input$no.fls.nurse.assessment.mins.other +
input$no.fls.nurse.recommendation.mins.other +
input$no.fls.nurse.monitoring.mins.other )*
 input$hourly.cost.nurse)+
((input$no.fls.doctor.identification.mins.other +
input$no.fls.doctor.assessment.mins.other +
input$no.fls.doctor.recommendation.mins.other +
input$no.fls.doctor.monitoring.mins.other )*
 input$hourly.cost.doctor)+
((input$no.fls.radiographer.identification.mins.other +
input$no.fls.radiographer.assessment.mins.other +
input$no.fls.radiographer.recommendation.mins.other +
input$no.fls.radiographer.monitoring.mins.other )*
 input$hourly.cost.radiographer)+
  ((input$no.fls.allied_health.identification.mins.other +
input$no.fls.allied_health.assessment.mins.other +
input$no.fls.allied_health.recommendation.mins.other +
input$no.fls.allied_health.monitoring.mins.other )*
 input$hourly.cost.allied_health)+
  ((input$no.fls.administrator.identification.mins.other +
input$no.fls.administrator.assessment.mins.other +
input$no.fls.administrator.recommendation.mins.other +
input$no.fls.administrator.monitoring.mins.other)*
 input$hourly.cost.administrator)#+
#   ((input$no.fls.other.identification.mins.other +
# input$no.fls.other.assessment.mins.other +
# input$no.fls.other.recommendation.mins.other +
# input$no.fls.other.monitoring.mins.other )*
#  input$hourly.cost.other)
) 
  
# if all zero
no.fls.other.staff.cost<-ifelse(length(no.fls.other.staff.cost)==0,
                              0, no.fls.other.staff.cost)




# fls 
fls.hip.staff.cost<-
(((input$fls.administrator.identification.mins.hip +
input$fls.administrator.assessment.mins.hip +
input$fls.administrator.recommendation.mins.hip +
input$fls.administrator.monitoring.mins.hip)*
 input$hourly.cost.administrator)+
((input$fls.fls_coordinator.identification.mins.hip +
input$fls.fls_coordinator.assessment.mins.hip +
input$fls.fls_coordinator.recommendation.mins.hip +
input$fls.fls_coordinator.monitoring.mins.hip )*
 input$hourly.cost.fls_coordinator)+
((input$fls.nurse.identification.mins.hip +
input$fls.nurse.assessment.mins.hip +
input$fls.nurse.recommendation.mins.hip +
input$fls.nurse.monitoring.mins.hip )*
 input$hourly.cost.nurse)+
((input$fls.doctor.identification.mins.hip +
input$fls.doctor.assessment.mins.hip +
input$fls.doctor.recommendation.mins.hip +
input$fls.doctor.monitoring.mins.hip )*
 input$hourly.cost.doctor)+
((input$fls.radiographer.identification.mins.hip +
input$fls.radiographer.assessment.mins.hip +
input$fls.radiographer.recommendation.mins.hip +
input$fls.radiographer.monitoring.mins.hip )*
 input$hourly.cost.radiographer)+
  ((input$fls.allied_health.identification.mins.hip +
input$fls.allied_health.assessment.mins.hip +
input$fls.allied_health.recommendation.mins.hip +
input$fls.allied_health.monitoring.mins.hip )*
 input$hourly.cost.allied_health)+
  ((input$fls.administrator.identification.mins.hip +
input$fls.administrator.assessment.mins.hip +
input$fls.administrator.recommendation.mins.hip +
input$fls.administrator.monitoring.mins.hip)*
 input$hourly.cost.administrator)#+
#   ((input$fls.other.identification.mins.hip +
# input$fls.other.assessment.mins.hip +
# input$fls.other.recommendation.mins.hip +
# input$fls.other.monitoring.mins.hip )*
#  input$hourly.cost.other)
)
# if all zero
fls.hip.staff.cost<-ifelse(length(fls.hip.staff.cost)==0,
                              0, fls.hip.staff.cost)

fls.spine.staff.cost<-
(((input$fls.administrator.identification.mins.spine +
input$fls.administrator.assessment.mins.spine +
input$fls.administrator.recommendation.mins.spine +
input$fls.administrator.monitoring.mins.spine)*
 input$hourly.cost.administrator)+
((input$fls.fls_coordinator.identification.mins.hip +
input$fls.fls_coordinator.assessment.mins.hip +
input$fls.fls_coordinator.recommendation.mins.hip +
input$fls.fls_coordinator.monitoring.mins.hip )*
 input$hourly.cost.fls_coordinator)+((input$fls.nurse.identification.mins.spine +
input$fls.nurse.assessment.mins.spine +
input$fls.nurse.recommendation.mins.spine +
input$fls.nurse.monitoring.mins.spine )*
 input$hourly.cost.nurse)+
((input$fls.doctor.identification.mins.spine +
input$fls.doctor.assessment.mins.spine +
input$fls.doctor.recommendation.mins.spine +
input$fls.doctor.monitoring.mins.spine )*
 input$hourly.cost.doctor)+
((input$fls.radiographer.identification.mins.spine +
input$fls.radiographer.assessment.mins.spine +
input$fls.radiographer.recommendation.mins.spine +
input$fls.radiographer.monitoring.mins.spine )*
 input$hourly.cost.radiographer)+
  ((input$fls.allied_health.identification.mins.spine +
input$fls.allied_health.assessment.mins.spine +
input$fls.allied_health.recommendation.mins.spine +
input$fls.allied_health.monitoring.mins.spine )*
 input$hourly.cost.allied_health)+
  ((input$fls.administrator.identification.mins.spine +
input$fls.administrator.assessment.mins.spine +
input$fls.administrator.recommendation.mins.spine +
input$fls.administrator.monitoring.mins.spine)*
 input$hourly.cost.administrator)#+
#   ((input$fls.other.identification.mins.spine +
# input$fls.other.assessment.mins.spine +
# input$fls.other.recommendation.mins.spine +
# input$fls.other.monitoring.mins.spine )*
#  input$hourly.cost.other)
)
# if all zero
fls.spine.staff.cost<-ifelse(length(fls.spine.staff.cost)==0,
                              0, fls.spine.staff.cost)

fls.other.staff.cost<-
(((input$fls.administrator.identification.mins.other +
input$fls.administrator.assessment.mins.other +
input$fls.administrator.recommendation.mins.other +
input$fls.administrator.monitoring.mins.other)*
 input$hourly.cost.administrator)+
((input$fls.fls_coordinator.identification.mins.hip +
input$fls.fls_coordinator.assessment.mins.hip +
input$fls.fls_coordinator.recommendation.mins.hip +
input$fls.fls_coordinator.monitoring.mins.hip )*
 input$hourly.cost.fls_coordinator)+
  ((input$fls.nurse.identification.mins.other +
input$fls.nurse.assessment.mins.other +
input$fls.nurse.recommendation.mins.other +
input$fls.nurse.monitoring.mins.other )*
 input$hourly.cost.nurse)+
((input$fls.doctor.identification.mins.other +
input$fls.doctor.assessment.mins.other +
input$fls.doctor.recommendation.mins.other +
input$fls.doctor.monitoring.mins.other )*
 input$hourly.cost.doctor)+
((input$fls.radiographer.identification.mins.other +
input$fls.radiographer.assessment.mins.other +
input$fls.radiographer.recommendation.mins.other +
input$fls.radiographer.monitoring.mins.other )*
 input$hourly.cost.radiographer)+
  ((input$fls.allied_health.identification.mins.other +
input$fls.allied_health.assessment.mins.other +
input$fls.allied_health.recommendation.mins.other +
input$fls.allied_health.monitoring.mins.other )*
 input$hourly.cost.allied_health)+
  ((input$fls.administrator.identification.mins.other +
input$fls.administrator.assessment.mins.other +
input$fls.administrator.recommendation.mins.other +
input$fls.administrator.monitoring.mins.other)*
 input$hourly.cost.administrator)#+
#   ((input$fls.other.identification.mins.other +
# input$fls.other.assessment.mins.other +
# input$fls.other.recommendation.mins.other +
# input$fls.other.monitoring.mins.other )*
#  input$hourly.cost.other)
)
  # if all zero
fls.other.staff.cost<-ifelse(length(fls.other.staff.cost)==0,
                              0, fls.other.staff.cost)



# cost when identified- cycle where identified 
# (was not identified in previous period OR
# new fx in current)
m.TR<-m.TR %>% 
  mutate(identified.lag=lag(identified))
#browser()
m.TR<-m.TR %>%  
mutate(fx_prev.staff.cost=
  ifelse(identified=="Yes" & 
           (identified.lag=="No"| c_af==1) &
         recent.fx=="hip" & intervention=="no FLS",
         no.fls.hip.staff.cost,       
  ifelse(identified=="Yes" & 
           (identified.lag=="No"| c_af==1) &
         recent.fx=="spine" & intervention=="no FLS",
         no.fls.spine.staff.cost,
  ifelse(identified=="Yes" & 
           (identified.lag=="No"| c_af==1) &
         recent.fx=="other" & intervention=="no FLS",
         no.fls.other.staff.cost,

  ifelse(identified=="Yes" & 
           (identified.lag=="No"| c_af==1) &
         recent.fx=="hip" & intervention=="FLS",
         fls.hip.staff.cost,       
  ifelse(identified=="Yes" & 
           (identified.lag=="No"| c_af==1) &
         recent.fx=="spine" & intervention=="FLS",
         fls.spine.staff.cost,
  ifelse(identified=="Yes" & 
           (identified.lag=="No"| c_af==1) &
         recent.fx=="other" & intervention=="FLS",
         fls.other.staff.cost,          
         0)))))))

m.TR<-m.TR %>% 
  select(-identified.lag)


# lab.test and dxa cost ------
m.TR<-m.TR %>% 
  mutate(lab.test.cost=
           ifelse(lab.test==2 & s_hf==1,
              input$cost.lab.test.hip,
          ifelse(lab.test==2 & s_sf==1,
              input$cost.lab.test.spine, 
          ifelse(lab.test==2 & s_of==1,
              input$cost.lab.test.other, 0  )))) %>% 
  mutate(dxa.cost=
           ifelse(dxa==2,
              input$cost.dxa, 0 ))





# total cost ------
m.TR$total.cost.excl.location<-
    m.TR$procedure.cost+
m.TR$hosp.cost+m.TR$comm.cost+
m.TR$clinic.cost+m.TR$temp.rehab.cost+
  m.TR$discharge.clinic.cost+
m.TR$medication.cost+m.TR$fx_prev.staff.cost+
  m.TR$lab.test.cost+m.TR$dxa.cost

m.TR$total.cost<-
    m.TR$procedure.cost+
m.TR$hosp.cost+m.TR$comm.cost+
m.TR$clinic.cost+m.TR$temp.rehab.cost+
  m.TR$location.cost+m.TR$discharge.clinic.cost+
m.TR$medication.cost+m.TR$fx_prev.staff.cost+
  m.TR$lab.test.cost+m.TR$dxa.cost
 

# summary output ----
#browser()
m.TR <-  split(m.TR,
               m.TR[,c('id','intervention')])
m.TR
})


get.m.TR.summary<-
  reactive({
    
m.TR<- get.m.TR()
m.TR<-plyr::ldply(m.TR, data.frame, .id=NULL)
m.TR<-m.TR %>% 
  mutate(intervention=ifelse(intervention=="no FLS", "Current practice",
                             "FLS")) 

microsim_pop<-get.microsim_pop() 


output<-list()

#identified  ------
# index_fx, sex
output[["summary.m.TR.identified.over_time.index_fx.sex"]]<-
m.TR %>% 
  ungroup() %>% 
  mutate(identified=factor(identified,   
                           levels=c("No", "Yes"))) %>% 
  group_by(month,intervention,index_fx, sex, identified, .drop=FALSE) %>% 
  tally(name="n.identified")%>% 
 # add total n
   left_join(
     microsim_pop %>% 
       group_by(index_fx, sex, .drop=FALSE) %>% 
       tally(name="n_microsim")) 
# index_fx
output[["summary.m.TR.identified.over_time.index_fx"]]<-
  m.TR %>% 
  ungroup() %>% 
  mutate(identified=factor(identified,   
                           levels=c("No", "Yes"))) %>% 
  group_by(month,intervention,index_fx, identified, .drop=FALSE) %>% 
  tally(name="n.identified")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(index_fx,.drop=FALSE) %>% 
      tally(name="n_microsim")) 
# sex
output[["summary.m.TR.identified.over_time.sex"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(identified=factor(identified,   
                           levels=c("No", "Yes"))) %>% 
  group_by(month,intervention, sex, identified, .drop=FALSE) %>% 
  tally(name="n.identified")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(sex, .drop=FALSE) %>% 
      tally(name="n_microsim")) 
# overall
output[["summary.m.TR.identified.over_time.overall"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(identified=factor(identified,   
                           levels=c("No", "Yes"))) %>% 
  group_by(month,intervention, identified, .drop=FALSE) %>% 
  tally(name="n.identified")%>% 
  mutate(n_microsim=microsim_pop %>%   tally(name="n_microsim") %>% pull())
  
# treat  ------
# index_fx, sex
output[["summary.m.TR.treat.over_time.index_fx.sex"]]<-
  m.TR %>% 
  ungroup() %>% 
  mutate(treat=factor(treat,   
                           levels=c("No", "Yes"))) %>% 
  group_by(month,intervention,index_fx, sex, treat, .drop=FALSE) %>% 
  tally(name="n.treat")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(index_fx, sex, .drop=FALSE) %>% 
      tally(name="n_microsim")) 
# index_fx
output[["summary.m.TR.treat.over_time.index_fx"]]<-
  m.TR %>% 
  ungroup() %>% 
  mutate(treat=factor(treat,   
                           levels=c("No", "Yes"))) %>% 
  group_by(month,intervention,index_fx, treat, .drop=FALSE) %>% 
  tally(name="n.treat")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(index_fx,.drop=FALSE) %>% 
      tally(name="n_microsim")) 
# sex
output[["summary.m.TR.treat.over_time.sex"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(treat=factor(treat,   
                           levels=c("No", "Yes"))) %>% 
  group_by(month,intervention, sex, treat, .drop=FALSE) %>% 
  tally(name="n.treat")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(sex, .drop=FALSE) %>% 
      tally(name="n_microsim")) 
# overall
output[["summary.m.TR.treat.over_time.overall"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(treat=factor(treat,   
                           levels=c("No", "Yes"))) %>% 
  group_by(month,intervention, treat, .drop=FALSE) %>% 
  tally(name="n.treat")%>% 
  mutate(n_microsim=microsim_pop %>%   tally(name="n_microsim") %>% pull())


# adhering  ------
# index_fx, sex
output[["summary.m.TR.adhering.over_time.index_fx.sex"]]<-
  m.TR %>% 
  ungroup() %>% 
  mutate(adhering=factor(adhering,   
                      levels=c("No", "Yes"))) %>% 
  group_by(month,intervention,index_fx, sex, adhering, .drop=FALSE) %>% 
  tally(name="n.adhering")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(index_fx, sex, .drop=FALSE) %>% 
      tally(name="n_microsim")) 
# index_fx
output[["summary.m.TR.adhering.over_time.index_fx"]]<-
  m.TR %>% 
  ungroup() %>% 
  mutate(adhering=factor(adhering,   
                      levels=c("No", "Yes"))) %>% 
  group_by(month,intervention,index_fx, adhering, .drop=FALSE) %>% 
  tally(name="n.adhering")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(index_fx,.drop=FALSE) %>% 
      tally(name="n_microsim")) 
# sex
output[["summary.m.TR.adhering.over_time.sex"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(adhering=factor(adhering,   
                      levels=c("No", "Yes"))) %>% 
  group_by(month,intervention, sex, adhering, .drop=FALSE) %>% 
  tally(name="n.adhering")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(sex, .drop=FALSE) %>% 
      tally(name="n_microsim")) 
# overall
output[["summary.m.TR.adhering.over_time.overall"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(adhering=factor(adhering,   
                      levels=c("No", "Yes"))) %>% 
  group_by(month,intervention, adhering, .drop=FALSE) %>% 
  tally(name="n.adhering")%>% 
  mutate(n_microsim=microsim_pop %>%   tally(name="n_microsim") %>% pull())



# apply.rr  ------
# index_fx, sex
output[["summary.m.TR.apply.rr.over_time.index_fx.sex"]]<-
  m.TR %>% 
  ungroup() %>% 
  mutate(apply.rr=factor(apply.rr,   
                      levels=c("No", "Yes"))) %>% 
  group_by(month,intervention,index_fx, sex, apply.rr, .drop=FALSE) %>% 
  tally(name="n.apply.rr")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(index_fx, sex, .drop=FALSE) %>% 
      tally(name="n_microsim")) 
# index_fx
output[["summary.m.TR.apply.rr.over_time.index_fx"]]<-
  m.TR %>% 
  ungroup() %>% 
  mutate(apply.rr=factor(apply.rr,   
                      levels=c("No", "Yes"))) %>% 
  group_by(month,intervention,index_fx, apply.rr, .drop=FALSE) %>% 
  tally(name="n.apply.rr")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(index_fx,.drop=FALSE) %>% 
      tally(name="n_microsim")) 
# sex
output[["summary.m.TR.apply.rr.over_time.sex"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(apply.rr=factor(apply.rr,   
                      levels=c("No", "Yes"))) %>% 
  group_by(month,intervention, sex, apply.rr, .drop=FALSE) %>% 
  tally(name="n.apply.rr")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(sex, .drop=FALSE) %>% 
      tally(name="n_microsim")) 
# overall
output[["summary.m.TR.apply.rr.over_time.overall"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(apply.rr=factor(apply.rr,   
                      levels=c("No", "Yes"))) %>% 
  group_by(month,intervention, apply.rr, .drop=FALSE) %>% 
  tally(name="n.apply.rr")%>% 
  mutate(n_microsim=microsim_pop %>%   tally(name="n_microsim") %>% pull())



# medication -----
output[["summary.m.TR.medication.over_time.index_fx.sex"]]<-m.TR %>% 
  ungroup() %>%
  mutate(medication=str_to_sentence(medication)) %>% 
  mutate(medication=factor(medication,   
                           levels=c("None", "Alendronate", 
                                    "Risedronate","Strontium",
                                    "Ibandronate","Denosumab", 
                                    "Zoledronate","Teriparatide",
                                    "Abaloparatide","Romo")))  %>% 
  group_by(month,intervention,index_fx, sex, medication, .drop=FALSE) %>% 
  tally(name="n.medication")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(index_fx, sex, .drop=FALSE) %>% 
      tally(name="n_microsim")) 

# index_fx
output[["summary.m.TR.medication.over_time.index_fx"]]<-
  m.TR %>% 
  ungroup() %>% 
  mutate(medication=str_to_sentence(medication)) %>% 
  mutate(medication=factor(medication,   
                           levels=c("None", "Alendronate", 
                                    "Risedronate","Strontium",
                                    "Ibandronate","Denosumab", 
                                    "Zoledronate","Teriparatide",
                                    "Abaloparatide","Romo")))  %>% 
  group_by(month,intervention,index_fx, medication, .drop=FALSE) %>% 
  tally(name="n.medication")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(index_fx,.drop=FALSE) %>% 
      tally(name="n_microsim")) 
# sex
output[["summary.m.TR.medication.over_time.sex"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(medication=str_to_sentence(medication)) %>% 
  mutate(medication=factor(medication,   
                           levels=c("None", "Alendronate", 
                                    "Risedronate","Strontium",
                                    "Ibandronate","Denosumab", 
                                    "Zoledronate","Teriparatide",
                                    "Abaloparatide","Romo")))  %>% 
  group_by(month,intervention, sex, medication, .drop=FALSE) %>% 
  tally(name="n.medication")%>% 
  # add total n
  left_join(
    microsim_pop %>% 
      group_by(sex, .drop=FALSE) %>% 
      tally(name="n_microsim")) 
# overall
output[["summary.m.TR.medication.over_time.overall"]]<- m.TR %>% 
  ungroup() %>% 
  mutate(medication=str_to_sentence(medication)) %>% 
  mutate(medication=factor(medication,   
                           levels=c("None", "Alendronate", 
                                    "Risedronate","Strontium",
                                    "Ibandronate","Denosumab", 
                                    "Zoledronate","Teriparatide",
                                    "Abaloparatide","Romo")))  %>% 
  group_by(month,intervention, medication, .drop=FALSE) %>% 
  tally(name="n.medication")%>% 
  mutate(n_microsim=microsim_pop %>%   tally(name="n_microsim") %>% pull())




# output -----
output

}) 


# SUMMARISE RESULTS ------
# markov trace -----
output$table.m.TR<-renderDataTable({
  m.TR<-get.m.TR()
  m.TR<-plyr::ldply(m.TR, data.frame, .id=NULL)
  # m.TR<-m.TR %>%  
  #   select(month, id, intervention,
  #          s_hf, s_sf, s_of, s_ff, s_d,
  #          identified, medication, adhering)
  datatable(m.TR,
            filter = "top",
             options = list(pageLength = 61))
  
  })


# summary of cohort -----
# get summary by fx, age and sex
summary.microsim.pop.age.sex.fx<-reactive({
microsim_pop<-get.microsim_pop()
microsim_pop %>% 
  group_by(index_fx,age,sex) %>% 
  tally()
})
# summary table for fx, age and sex
output$table.summary.microsim.pop.age.sex.fx <-
renderText({
# browser()
study_pop_n<-get.study_pop_n.age() 
  
spine_fx_n.male<-as.numeric(study_pop_n %>% filter(names=="spine_fx_n.male") %>% 
  select(n))
hip_fx_n.male<-as.numeric(study_pop_n %>% filter(names=="hip_fx_n.male") %>% 
  select(n))
other_fx_n.male<-as.numeric(study_pop_n %>% filter(names=="other_fx_n.male") %>% 
  select(n))
spine_fx_n.female<-as.numeric(study_pop_n %>% filter(names=="spine_fx_n.female") %>% 
  select(n))
hip_fx_n.female<-as.numeric(study_pop_n %>% filter(names=="hip_fx_n.female") %>% 
  select(n))
other_fx_n.female<-as.numeric(study_pop_n %>% filter(names=="other_fx_n.female") %>% 
  select(n))

  
summary.microsim.pop.age.sex.fx<-summary.microsim.pop.age.sex.fx()
summary.microsim.pop.age.sex.fx<-summary.microsim.pop.age.sex.fx %>%
inner_join(
# target population
data.frame(index_fx=c("hip", "hip","other","other",
                      "spine","spine"),
           sex=c("female", "male", "female", "male","female", "male"),
           target.n=c(hip_fx_n.female,hip_fx_n.male,
                      other_fx_n.female, other_fx_n.male,
                      spine_fx_n.female, spine_fx_n.male))) 

summary.microsim.pop.age.sex.fx<-summary.microsim.pop.age.sex.fx %>% 
  mutate(n=nice.num.count(n)) %>% 
  mutate(target.n=nice.num.count(target.n)) 

summary.microsim.pop.age.sex.fx<-summary.microsim.pop.age.sex.fx %>% 
  ungroup() %>% 
  mutate(index_fx=factor(str_to_sentence(index_fx),  
                         levels=c("Spine", "Hip", 
                                  "Other"))) %>% 
  mutate(sex=factor(str_to_sentence(sex),   
                         levels=c("Male", "Female")))  %>% 
  arrange(sex,index_fx) 

kable(summary.microsim.pop.age.sex.fx,
      col.names = c("Index fracture",
                    "Age",
                    "Sex", 
                    "N microsimulation cohort",
                    "N target population")) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))
 
})










# subsequent fractures ----
output$sec.frac.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.second.fx(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})



# mc error for subsequent fractures ----
output$mc.error.sec.frac<-
  renderPlot({
mc.error.second.fx<-lapply(get.m.TR(),
      function(df) {
        sec.frac <- data.frame( 
                   sec.frac=ifelse(df$h_af[which(df$month==60)] 
                                       # ie history based on months 0 to 59 
                                       >1 | df$c_af[which(df$month==60)]   
                                     # ie current fx month 60?
                            >1  , 1,0),
                     id=df$id[1],
                     index_fx=df$index_fx[1],
                     sex=df$sex[1],
                     intervention=df$intervention[1])
                   sec.frac})

mc.error.second.fx<-plyr::ldply(mc.error.second.fx,  
                      data.frame, .id=NULL)

mc.error.second.fx<-mc.error.second.fx %>% 
  group_by(intervention, index_fx, sex) %>% 
  mutate(seq=1:length(id))

mc.error.second.fx<-mc.error.second.fx %>% 
  group_by(intervention, index_fx, sex) %>% 
  arrange(intervention, index_fx, sex) %>% 
  mutate(convergence=cumsum(sec.frac)/seq) 
#browser()
if(max(mc.error.second.fx$seq)>100){
  mc.error.second.fx<-mc.error.second.fx %>% 
  filter(seq>50)
  
}


mc.fx.plot<-mc.error.second.fx %>% 
  ggplot(aes(group=intervention, colour=intervention))+
  facet_grid(index_fx~sex)+
  geom_line(aes(seq,convergence))

gg.general.format(mc.fx.plot) +
  ylab("Proportion with 2nd fracture")+
  xlab("Number of individuals")

     })

# deaths -------
output$deaths.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.deaths(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})

output$mc.error.deaths<-
  renderPlot({
mc.error.death<-lapply(get.m.TR(),
      function(df) {
        death <- data.frame( 
                   death=ifelse(df$s_d[which(df$month==60)]
                                       ==1, 1,0),
                     id=df$id[1],
                     index_fx=df$index_fx[1],
                     sex=df$sex[1],
                     intervention=df$intervention[1])
                   death})

mc.error.death<-plyr::ldply(mc.error.death,  
                      data.frame, .id=NULL)

mc.error.death<-mc.error.death %>% 
  group_by(intervention, index_fx, sex) %>% 
  mutate(seq=1:length(id))

mc.error.death<-mc.error.death %>% 
  group_by(intervention, index_fx, sex) %>% 
  arrange(intervention, index_fx, sex) %>% 
  mutate(convergence=cumsum(death)/seq) 

mc.fx.plot<-mc.error.death %>% 
  filter(seq>50) %>% 
  ggplot(aes(group=intervention, colour=intervention))+
  facet_grid(index_fx~sex)+
  geom_line(aes(seq,convergence))

gg.general.format(mc.fx.plot) +
  ylab("Proportion died")+
  xlab("Number of individuals")

     })


# c inc ------
output$plot.c.inc.sec.fx<-
  renderPlot({
   #browser()
  c.inc.plot<-get.c.inc.plot(get.m.TR(), input)
   c.inc.plot<-gg.general.format(c.inc.plot)
   c.inc.plot
  })

output$plot.hist.subs.fx<-
  renderPlot({
      hist.subs.fx<-get.hist.subs.fx1(get.m.TR(), input, get.microsim_pop())
   hist.subs.fx

 })

output$plot.c.inc.deaths<-
  renderPlot({
    c.inc.plot<-get.c.inc.deaths.plot(get.m.TR(), input)
   c.inc.plot<-gg.general.format(c.inc.plot)
   c.inc.plot
  })

# hcru ----


output$summary.procedures<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.procedures(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})


output$hosp.los.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.hosp.los(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})

# costs ----- 
output$total.cost.excl.location.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.total.cost.excl.location(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})

output$total.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.total.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})

output$procedure.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.procedure.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})
output$hosp.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.hosp.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})

output$comm.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.comm.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})
output$clinic.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.clinic.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})
output$temp.rehab.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.temp.rehab.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})
output$location.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.location.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})
output$discharge.clinic.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.discharge.clinic.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})
output$medication.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.medication.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})
output$fx_prev.staff.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.fx_prev.staff.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})
output$lab.test.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.lab.test.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})
output$dxa.cost.summary<-
renderText({
  
  m.TR<-get.m.TR()
  microsim_pop<-get.microsim_pop()
  study_pop_n<-get.study_pop_n()
  
  get.summary.dxa.cost(m.TR,
                      microsim_pop,
                      study_pop_n,
                      input)
})



# identified ----
get.identified.plot<-reactive({ 
  output<-get.m.TR.summary()

if(input$choose.identified.plot=="Overall"){
identified.plot<- output$summary.m.TR.identified.over_time.overall %>%  
    ggplot()+
    facet_grid(.~ intervention)+
    geom_col(aes(month, n.identified/n_microsim, 
                 fill=as.character(identified)), width=1)
}

if(input$choose.identified.plot=="By sentinel fracture"){
  identified.plot<-  output$summary.m.TR.identified.over_time.index_fx %>%  
    ggplot()+
    facet_grid(index_fx~ intervention)+
    geom_col(aes(month, n.identified/n_microsim, 
                 fill=as.character(identified)), width=1) 
}

if(input$choose.identified.plot=="By sex"){
  identified.plot<- output$summary.m.TR.identified.over_time.sex %>%  
    ggplot()+
    facet_grid(sex~ intervention)+
    geom_col(aes(month, n.identified/n_microsim, 
                 fill=as.character(identified)), width=1) 
}

if(input$choose.identified.plot=="By sentinel fracture and sex"){
identified.plot<- output$summary.m.TR.identified.over_time.index_fx.sex %>%  
  ggplot()+
  facet_grid(index_fx+ sex~ intervention)+
  geom_col(aes(month, n.identified/n_microsim, 
               fill=as.character(identified)), width=1) 
}
  
identified.plot<-gg.general.format(identified.plot)
identified.plot<-gg.add.colours.y_n(identified.plot)
identified.plot

 })

output$identified.plot<-renderPlot({
  get.identified.plot()
})

identified.plot.plotHeight<-reactive({
  rows<-(length(unique(ggplot_build(get.identified.plot())$data[[1]]$PANEL))/2)
  
  if(rows==1){
    350
  } else {
   rows*275 
  }

}) 

output$identified.plot.ui <- renderUI({
  plotOutput("identified.plot", height = identified.plot.plotHeight())
  })


# treat ----
get.treat.plot<-reactive({ 
  output<-get.m.TR.summary()
  
  if(input$choose.treat.plot=="Overall"){
    treat.plot<- output$summary.m.TR.treat.over_time.overall %>%  
      ggplot()+
      facet_grid(.~ intervention)+
      geom_col(aes(month, n.treat/n_microsim, 
                   fill=as.character(treat)), width=1)
  }
  
  if(input$choose.treat.plot=="By sentinel fracture"){
    treat.plot<-  output$summary.m.TR.treat.over_time.index_fx %>%  
      ggplot()+
      facet_grid(index_fx~ intervention)+
      geom_col(aes(month, n.treat/n_microsim, 
                   fill=as.character(treat)), width=1) 
  }
  
  if(input$choose.treat.plot=="By sex"){
    treat.plot<- output$summary.m.TR.treat.over_time.sex %>%  
      ggplot()+
      facet_grid(sex~ intervention)+
      geom_col(aes(month, n.treat/n_microsim, 
                   fill=as.character(treat)), width=1) 
  }
  
  if(input$choose.treat.plot=="By sentinel fracture and sex"){
    treat.plot<- output$summary.m.TR.treat.over_time.index_fx.sex %>%  
      ggplot()+
      facet_grid(index_fx+ sex~ intervention)+
      geom_col(aes(month, n.treat/n_microsim, 
                   fill=as.character(treat)), width=1) 
  }
  
  treat.plot<-gg.general.format(treat.plot)
  treat.plot<-gg.add.colours.y_n(treat.plot)
  treat.plot
  
})

output$treat.plot<-renderPlot({
  get.treat.plot()
})

treat.plot.plotHeight<-reactive({
  rows<-(length(unique(ggplot_build(get.treat.plot())$data[[1]]$PANEL))/2)
  
  if(rows==1){
    350
  } else {
    rows*275 
  }
  
}) 

output$treat.plot.ui <- renderUI({
  plotOutput("treat.plot", height = treat.plot.plotHeight())
})


# medication ----
get.medication.plot<-reactive({ 
  output<-get.m.TR.summary()
  if(input$choose.medication.plot=="Overall"){
    medication.plot<- output$summary.m.TR.medication.over_time.overall   %>%
      ggplot()+
      facet_grid(.~ intervention)+
      geom_col(aes(month, n.medication/n_microsim, 
                   fill=medication), width=1)
  }
  
  if(input$choose.medication.plot=="By sentinel fracture"){
    medication.plot<-  output$summary.m.TR.medication.over_time.index_fx %>% 
      ggplot()+
      facet_grid(index_fx~ intervention)+
      geom_col(aes(month, n.medication/n_microsim, 
                   fill=medication), width=1) 
  }
  
  if(input$choose.medication.plot=="By sex"){
    medication.plot<- output$summary.m.TR.medication.over_time.sex %>% 
      ggplot()+
      facet_grid(sex~ intervention)+
      geom_col(aes(month, n.medication/n_microsim, 
                   fill=medication), width=1) 
  }
  
  if(input$choose.medication.plot=="By sentinel fracture and sex"){
    medication.plot<- output$summary.m.TR.medication.over_time.index_fx.sex %>%
      ggplot()+
      facet_grid(index_fx+ sex~ intervention)+
      geom_col(aes(month, n.medication/n_microsim, 
                   fill=medication), width=1) 
  }
  
  medication.plot<-gg.general.format(medication.plot)
  medication.plot<-gg.add.colours.meds(medication.plot)
  medication.plot

})

output$medication.plot<-renderPlot({
  get.medication.plot()
})

medication.plot.plotHeight<-reactive({
  rows<-(length(unique(ggplot_build(get.medication.plot())$data[[1]]$PANEL))/2)
  
  if(rows==1){
    350
  } else {
    rows*275 
  }
  
}) 

output$medication.plot.ui <- renderUI({
  plotOutput("medication.plot", height = medication.plot.plotHeight())
})



# adhering ----
get.adhering.plot<-reactive({ 
  output<-get.m.TR.summary()
  
  if(input$choose.adhering.plot=="Overall"){
    adhering.plot<- output$summary.m.TR.adhering.over_time.overall %>%  
      ggplot()+
      facet_grid(.~ intervention)+
      geom_col(aes(month, n.adhering/n_microsim, 
                   fill=as.character(adhering)), width=1)
  }
  
  if(input$choose.adhering.plot=="By sentinel fracture"){
    adhering.plot<-  output$summary.m.TR.adhering.over_time.index_fx %>%  
      ggplot()+
      facet_grid(index_fx~ intervention)+
      geom_col(aes(month, n.adhering/n_microsim, 
                   fill=as.character(adhering)), width=1) 
  }
  
  if(input$choose.adhering.plot=="By sex"){
    adhering.plot<- output$summary.m.TR.adhering.over_time.sex %>%  
      ggplot()+
      facet_grid(sex~ intervention)+
      geom_col(aes(month, n.adhering/n_microsim, 
                   fill=as.character(adhering)), width=1) 
  }
  
  if(input$choose.adhering.plot=="By sentinel fracture and sex"){
    adhering.plot<- output$summary.m.TR.adhering.over_time.index_fx.sex %>%  
      ggplot()+
      facet_grid(index_fx+ sex~ intervention)+
      geom_col(aes(month, n.adhering/n_microsim, 
                   fill=as.character(adhering)), width=1) 
  }
  
  adhering.plot<-gg.general.format(adhering.plot)
  adhering.plot<-gg.add.colours.y_n(adhering.plot)
  adhering.plot
  
})

output$adhering.plot<-renderPlot({
  get.adhering.plot()
})

adhering.plot.plotHeight<-reactive({
  rows<-(length(unique(ggplot_build(get.adhering.plot())$data[[1]]$PANEL))/2)
  
  if(rows==1){
    350
  } else {
    rows*275 
  }
  
}) 

output$adhering.plot.ui <- renderUI({
  plotOutput("adhering.plot", height = adhering.plot.plotHeight())
})



# apply.rr ----
get.apply.rr.plot<-reactive({ 
  output<-get.m.TR.summary()
  
  if(input$choose.apply.rr.plot=="Overall"){
    apply.rr.plot<- output$summary.m.TR.apply.rr.over_time.overall %>%  
      ggplot()+
      facet_grid(.~ intervention)+
      geom_col(aes(month, n.apply.rr/n_microsim, 
                   fill=as.character(apply.rr)), width=1)
  }
  
  if(input$choose.apply.rr.plot=="By sentinel fracture"){
    apply.rr.plot<-  output$summary.m.TR.apply.rr.over_time.index_fx %>%  
      ggplot()+
      facet_grid(index_fx~ intervention)+
      geom_col(aes(month, n.apply.rr/n_microsim, 
                   fill=as.character(apply.rr)), width=1) 
  }
  
  if(input$choose.apply.rr.plot=="By sex"){
    apply.rr.plot<- output$summary.m.TR.apply.rr.over_time.sex %>%  
      ggplot()+
      facet_grid(sex~ intervention)+
      geom_col(aes(month, n.apply.rr/n_microsim, 
                   fill=as.character(apply.rr)), width=1) 
  }
  
  if(input$choose.apply.rr.plot=="By sentinel fracture and sex"){
    apply.rr.plot<- output$summary.m.TR.apply.rr.over_time.index_fx.sex %>%  
      ggplot()+
      facet_grid(index_fx+ sex~ intervention)+
      geom_col(aes(month, n.apply.rr/n_microsim, 
                   fill=as.character(apply.rr)), width=1) 
  }
  
  apply.rr.plot<-gg.general.format(apply.rr.plot)
  apply.rr.plot<-gg.add.colours.y_n(apply.rr.plot)
  apply.rr.plot
  
})

output$apply.rr.plot<-renderPlot({
  get.apply.rr.plot()
})

apply.rr.plot.plotHeight<-reactive({
  rows<-(length(unique(ggplot_build(get.apply.rr.plot())$data[[1]]$PANEL))/2)
  
  if(rows==1){
    350
  } else {
    rows*275 
  }
  
}) 

output$apply.rr.plot.ui <- renderUI({
  plotOutput("apply.rr.plot", height = apply.rr.plot.plotHeight())
})



## rr.applied ------
output$table.rr.applied<-
renderText({
  rr.applied<-plyr::ldply(get.m.TR(), data.frame, .id=NULL)
kable(rr.applied %>%
  group_by(month,intervention, index_fx, sex) %>%
  summarise(sum(!is.na(apply.rr)))) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))
})

# plot applied tps ----
 output$tps.apply.rr.plot<-renderPlot({
apply.rr<-plyr::ldply(get.m.TR(), data.frame, .id=NULL)
apply.rr$apply.rr<-ifelse(is.na(apply.rr$apply.rr), "No","Yes")

 apply.rr %>%
   group_by(month, intervention, apply.rr) %>% 
   summarise(mean.sf.tp=mean(sf.tp)) %>% 
   ggplot()+
   geom_point(aes(month, mean.sf.tp, colour=apply.rr))
 
 
 
 
 })


}

#### RUN APP ----

shinyApp(ui = ui, server = server)


