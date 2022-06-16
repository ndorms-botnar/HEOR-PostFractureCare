# Health Economics & Outcomes Research - CSM, NDORMS

# PfcCalculator

## Setting up RStudio to run the model
The code is to be run from RStudio, with the following packages required to be installed:
- dplyr  
- tidyr  
- purrr  
- here  
- stringr  
- scales  
- ggplot2  
- kableExtra  
- shiny  
- shinythemes  
- shinyjs  
- shinyWidgets  
- shinycssloaders  
- data.table  
- dtplyr  
- Hmisc  
- kableExtra  
- stringr  
- scales  
- DT  
- withr   
- rms  
- broom  
- rmarkdown  

These can be installed from CRAN by running 
```
install.packages("name of package")
```

## Bringing in the code from GitHub  
You can download the folder containing all the code by selecting Code (the green button above) -> Download ZIP.  
Once you have this  
- unzip  
- open PfcCalculator.Rproj (this should open in rstudio, with you entering the package - you will see "PfcCalculator" on the top-right of RStudio if this has worked).  

## Once inside the `HEOR@PostFractureCare.Rproj`, open R microsim/extras/CodeToRun.R
The only file you should need to interact with directly is *R microsim/extras/CodeToRun.R* (well, unless if you want to change the number of simulations being run etc). Once inside the Rproj, you can then open this file. 

## Specify country of interest 
At the top of *R microsim/extras/CodeToRun.R* you need to specify the country for which the model is to be run for.  

## Load packages 
Next, in *R microsim/extras/CodeToRun.R*, you will need to load the required packages. If installed (as above), now it should just be a matter of loading them.   

## If required, prepare model inputs  
To set up the inputs for a country, you can then launch the RShiny app locally by uncommenting the following code  
```
shiny::runApp(here("RShiny"))
```
Once the app loads, you will need to at a minimum choose the country and region (in the "Setting" page) and specify the number of sentinel fractures (note if selecting general population, you will need to make sure to view this input). Other inputs can be edited, but otherwise will be set as their defaults.  
You can then navigate to Results-> Study population to check the population being generated, and then to save the inputs go to Results-> Export model inputs and click "Download" (this will save the inputs locally- note, for a given country, this will overwrite any previous model inputs for that country).
Once the inputs are downloaded, you can then close the app.  

Note, to change the number of simulations being run, before launching the app open *PfcCalculator/RShiny/app.R* and change 
```
n_microsimulation<- ...
```
on line 26 to the number of simulations you want to be run.   

## Run the model
After this, all the rest of the code in *R microsim/extras/CodeToRun COUNTRY_NAME .R* should work as is. The code will do the following  
- load functions required to run the model  
- set where to save model output (note, everything will be saved in the same folder as that containing the Rproj)  
- run the model 
- gets the summary output 
- writes the report summarising the results (this will be saved in a subfolder of *PfcCalculator/report/* corresponding to the coutry of interest  