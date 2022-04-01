# This code:
# > downloads 2000 Brazilian Census microdata from IBGE
# > unzip the microdata
# > reads .txt microdata into data frame
# > saves data sets as .csv files



# 24-Nov-2016
# By, Rafael Pereira
# R version:  RRO 3.3.2 (64 bits)
# you can fund my contacts at www.urbandemographics.blogspot.com


############################################################################
## ATTENTION: This is the only modification you have to do in this script, I hope ;)
setwd("R:/Dropbox/bases_de_dados/censo_demografico/censo_2000") # set working Directory

setwd("Z:/Desktop/ineq_LifeExp shared") # set working Directory






### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
##################### Load packages -------------------------------------------------------

source("./Rscripts/_LoadPackages.R")




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
##################### Create subdirectories -------------------------------------------------------

# create subdirectories where files will be saved
  dir.create(file.path(".", "data"))
  dir.create(file.path("./data", "dados_txt2000"))

  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  ######## Download Census DATA -----------------------------------------------------
  
  # using package 'microdadosBrasil'
  download_sourceData("CENSO", 2000, unzip = T, dest = "./dados_txt2000")
  
  
  
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
  ######## Prepare Documentation files to read .txt -----------------------------------------------------
  
  
  # Docs to correct decimal points
  
  # Open variables layout from Excel file
  dic_dom <- get_import_dictionary("CENSO", 2000, "domicilios") %>% setDT()
  dic_pes <- get_import_dictionary("CENSO", 2000, "pessoas") %>% setDT()
  
  
  
  # Indicate which columns will be read from .txt files
  myvariblesDOM <- c( "V0102" # state
                      , "V1001" # Region
                      , "V0103" # municipality
                      , "V0300" # controle - household id
                      #, "V0400" # person order
                      , "AREAP" # Sampling area - Area de ponderacao
                      , "V1006" # urban x rural
                      , "V7100" # Number of people in the household
                      , "V7616" # total household income (monthly)
                      , "PESO_DOMIC"  # weight
                      , "V7203" # dwellers density21
                      , "V7204" # dwellers density2
                      , "V7617" # household salaries
  )
  
  myvariblesPES <- c( "V0102" # state
                      , "V1001" # Region
                      , "V0103" # municipality
                      , "V0300" # controle - household id
                      #, "V0400" # person order
                      , "V0401" # sex
                      , "V4752" # Age
                      , "V0408" # race
                      , "V4614" # total income
                      , "AREAP" # Sampling area - Area de ponderacao
                      , "V1006" # urban x rural
                      , "PES_PESSOA"  # weight
                      # , "V4514" # Total de rendimentos no trabalho principal, em salários mínimos
                      # , "V4524" # Total de rendimentos nos demais trabalhos, em salários mínimos
                      # , "V4526" # Total de rendimentos em todos os trabalhos, em salários mínimos
                      , "V4615" # Total de rendimentos, em salários mínimos
  )
  
  
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Read and save HOUSEHOLD Files (29 seconds)  ----------------------------------------------------

# read data
  censo2000_BRdom <- read_CENSO('domicilios', 2000,
                                vars_subset = myvariblesDOM, 
                                root_path = "./data/dados_txt2000")
  
# Update decimals in the data
  var.decimals <- dic_dom[decimal_places > 0, ] # identify variables with decimals to update
  var.decimals <- var.decimals[, c("var_name","decimal_places"), with = FALSE]
  var <-  dput(var.decimals$var_name) # list of variables to update decimals
  
  # Update decimals in the data
  setDT(censo2000_BRdom)
    for(j in seq_along(var)){
      set(censo2000_BRdom, i=NULL, j=var[j], value=as.numeric(censo2000_BRdom[[var[j]]])/10^var.decimals[, decimal_places][j])
    }
    
  sum(censo2000_BRdom$PESO_DOMIC, na.rm = T) # it should return 45,507,516
  
# Save national dataset
  save(censo2000_BRdom, file="./data/censo2000_BRdom.Rdata")
  
  rm(censo2000_BRdom)
  gc(reset = T)
  beep()






  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Read and save PERSON Files  (2 minutes) ----------------------------------------------------
 
  # read data 
  censo2000_BRpes <- read_CENSO('pessoas', 2000,
                                vars_subset = myvariblesPES, 
                                root_path = "./data/dados_txt2000")
  # 2.56 GB in memory
  object.size(censo2000_BRpes) /1000000000
  
# Prepare documentation to Update decimals in the data
  var.decimals <- dic_pes[decimal_places > 0, ] # identify variables with decimals to update
  var.decimals <- var.decimals[, c("var_name","decimal_places"), with = FALSE]
  var <-  dput(var.decimals$var_name) # list of variables to update decimals
  
  # Update decimals in the data
  setDT(censo2000_BRpes)
  for(j in seq_along(var)){
    set(censo2000_BRpes, i=NULL, j=var[j], value=as.numeric(censo2000_BRpes[[var[j]]])/10^var.decimals[, decimal_places][j])
  }
  
  
  sum(censo2000_BRpes$PES_PESSOA, na.rm = T) # it should return 169,872,856
  
  
# Save national dataset
  save(censo2000_BRpes, file="./data/censo2000_BRpes.Rdata")
  beep()
  

 

# Clean Global Env.
 rm(list=ls())
 gc(reset = T)
 