

# check installation of packages
  list.of.packages <- c('dicionariosIBGE', 'magrittr', 'data.table', 'readr', 'Hmisc', 'bit64',
                        'readr', 'readxl', 'beepr', 'dplyr', 'survey', 'ggplot2', 'dplyr', 'ineq', 
                        'RColorBrewer', 'viridis', 'gridExtra', 'ggthemes', 'cowplot', 'devtools', 'stringi')

  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
   if(length(new.packages)) install.packages(new.packages)


  
  
# check installation of microdadosBrasil package
  library(devtools)
  library(stringi) 
  new.packages <- !('microdadosBrasil' %in% installed.packages()[,"Package"])
  if(length(new.packages)) devtools::install_github("lucasmation/microdadosBrasil")


  # check installation of DDM package, for for indirect estimation of death registration coverage
  new.packages <- !('DDM' %in% installed.packages()[,"Package"])
  if(length(new.packages)) devtools::install_github("timriffe/AdultCoverage/AdultCoverage/R/DDM")
  
  

##################### Load packages -------------------------------------------------------

library(microdadosBrasil)
library(DDM)
library(dicionariosIBGE)
library(magrittr)     # using pipes %>%
library(data.table)   # to manipulate data frames (fread is ultrafast for reading CSV files)
library(readr)        #fast read of fixed witdh files
library(readxl)       # read excel spreadsheets
library(beepr)        # Beeps at the end of the command
library(survey)       #complex sample surveys
library(ggplot2)      # to use google map tiles
library(dplyr)        # to manipulate data frames
library(ineq)         # Inequality and concentration   measures
library(RColorBrewer) # nice color palettes
library(viridis)      # nice color palettes
library(gridExtra)    # Arrange Grid of ggplots
library(ggthemes)     # themes for ggplot
library(cowplot)      # arrange ggplots
library(Hmisc)        # calculate weighted percentiles 
library(bit64)        # work with 64bit integers
  
  
options(digits=3)   # number of digits to show
options(scipen=999) # disable scientific notation

rm(new.packages, list.of.packages)
