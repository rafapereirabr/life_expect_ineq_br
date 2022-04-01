# This code:
  # 1 downloads 2010 Brazilian Census microdata
  # 2 unzip the microdata
  # 3 reads seleected variables from .txt microdata into data frame
  # 4 saves data sets as .csv files



# Data: 2010 Demographic Census, Brazil (IBGE)
# By, Rafael Pereira
# 02-Oct-2015, Oxford
# R version:  RRO 3.2.2 (64 bits)


# set working Directory
  # setwd("R:/Dropbox/bases_de_dados/censo_demografico/censo_2010") # set working Directory
setwd("R:/Dropbox/life expectancy (inequality)/income/ineq_LifeExp shared") # set working Directory




##################### Load packages -------------------------------------------------------

source("./Rscripts/_LoadPackages.R")



  

######## Download Census DATA -----------------------------------------------------

# create subdirectories where we'll save files
  dir.create(file.path(".", "data"))
  dir.create(file.path("./data", "dados_txt2010"))
  


######## Download Census DATA -----------------------------------------------------

# using package 'microdadosBrasil'
  download_sourceData("CENSO", 2010, unzip = T, root_path  = "./data/dados_txt2010")




# ######## Prepare Documentation to correct decimal points -----------------------------------------------------
# 
# # Open variables layout from Excel file
#   Layout_microdados <- list.files("./data/dados_txt2010/Microdados/Documentacao", pattern = "Layout_microdados_Amostra.xls", full.names = TRUE, recursive=T)
#   dic_dom <- read_excel( Layout_microdados, sheet =1, skip = 1)
#   dic_pes <- read_excel( Layout_microdados, sheet =2, skip = 1)
#   dic_mor <- read_excel( Layout_microdados, sheet =4, skip = 1)
#   
# 
# # convert to data table
#   setDT(dic_dom)
#   setDT(dic_pes)
#   setDT(dic_mor)
#   
# # Create function to compute Decimal
#   computeDescimals <- function(dataset){dataset[is.na(DEC), DEC := 0] }
# # Apply function
#   lapply(list(dic_dom,dic_pes,dic_mor), computeDescimals)
#   
  
  
  
######## Select columns to read from .txt -----------------------------------------------------


    # Specify Variables to be loaded
      myvariblesPES <- c("V0001", # State
                         "V0002", # Municipality
                         "V1001", # Region
                         "V0011", # Sampling Area (AREA DE PONDERACAO)
                         "V0300", # Control variable
                         "V6525", # Personal income from all sources 
                         "V0502", # head of the household == 1
                         "V0601", # Sex
                         "V6036", # Age
                        # "V6400", # educational attainment
                        # "V0606", # race
                         "V6531", # Household income per capita
                         "V5030", # single person household? 1==yes
                        # "V6664", # Had a borned child (alive) in previous 12 months
                         "V4001", # imputacao do domilicio
                         "V0010") # Weight
      
  
    
      myvariblesDOM <- c("V0001", # State
                         "V0002", # Municipality
                         "V1001", # Region
                         "V6600", # single-person households ==1
                         "V0011", # Sampling Area (?REA DE PONDERA??O)
                         "V0300", # Control variable
                         "V6531", # Household income per capita
                         "V6529", # Total household income
                         "V0401", # number of people in the household
                         "V4001", # imputacao do domilicio
                         
                         "V0010")  # Weight
      
      myvariblesMOR <- c("V0001", # State
                         "V0002", # Municipality
                         "V1001", # Region
                         "V0704", # Sex
                         "V7051", # age in years at death
                         "V7052", # age in months at death (only infant mortality)
                         "V0011", # Sampling Area (?REA DE PONDERA??O)
                         "V0300", # Control variable
                         "V4001", # imputacao do domilicio
                         "V0010")  # Weight
    
      

### MORTALITY Files (2 second) -----------------------------------------------------

# read data
  censo2010_BRdeaths <- read_CENSO('mortalidade', 2010,
                                 vars_subset = myvariblesMOR, 
                                 root_path = "./data/dados_txt2010")


# # Update decimals in the data
#   var.decimals <- dic_mor[DEC > 0, ] # identify variables with decimals to update
#   var.decimals <- var.decimals[, c("VAR","DEC"), with = FALSE]
#   var <-  dput(var.decimals$VAR) # list of variables to update decimals
# 
# # Update decimals in the data
#   setDT(censo2010_BRdeaths)
#   
#   for(j in seq_along(var)){
#     set(censo2010_BRdeaths, i=NULL, j=var[j], value=as.numeric(censo2010_BRdeaths[[var[j]]])/10^var.decimals[, DEC][j])
#     }
  
  sum(censo2010_BRdeaths$V0010) # it should return 1,025,029

# Save national dataset
  save(censo2010_BRdeaths, file="./data/censo2010_BRdeaths.Rdata")
  
  rm(censo2010_BRdeaths)
  gc(reset = T)


  





### HOUSEHOLD Files  (15 seconds) ----------------------------------------------------

# read data
  censo2010_BRdom <- read_CENSO('domicilios', 2010,
                                vars_subset = myvariblesDOM, 
                                root_path = "./data/dados_txt2010")

  
# # Update decimals in the data
#   var.decimals <- dic_dom[DEC > 0, ] # identify variables with decimals to update
#   var.decimals <- var.decimals[, c("VAR","DEC"), with = FALSE]
#   var <-  dput(var.decimals$VAR) # list of variables to update decimals
# 
# # Update decimals in the data
#   setDT(censo2010_BRdom)
#   
#   for(j in seq_along(var)){
#     set(censo2010_BRdom, i=NULL, j=var[j], value=as.numeric(censo2010_BRdom[[var[j]]])/10^var.decimals[, DEC][j])
#   }
  
  sum(censo2010_BRdom$V0010) # it should return 58,051,449


    
# Save national data set as a '.csv' file
  save(censo2010_BRdom, file="./data/censo2010_BRdom.Rdata")
  rm(censo2010_BRdom)
  gc(reset = T)





######## INDIVIDUALS Files  (3 minutes) ----------------------------------------------------

  
# aaa <-   c("V0001", "V6531", "V0010")
  
  
  # read data
  censo2010_BRpes <- read_CENSO('pessoas', 2010, 
                                vars_subset = myvariblesPES, 
                                root_path = "./data/dados_txt2010")
  
  
# # Update decimals in the data
#   var.decimals <- dic_pes[DEC > 0, ] # identify variables with decimals to update
#   var.decimals <- var.decimals[, c("VAR","DEC"), with = FALSE]
#   var <-  dput(var.decimals$VAR) # list of variables to update decimals
#   
# # Update decimals in the data
#   setDT(censo2010_BRpes)
#   
#   for(j in seq_along(var)){
#     set(censo2010_BRpes, i=NULL, j=var[j], value=as.numeric(censo2010_BRpes[[var[j]]])/10^var.decimals[, DEC][j])
#   }
  
  sum(censo2010_BRpes$V0010) # it should return 190,755,799
  
  
# Save national dataset
  save(censo2010_BRpes, file="./data/censo2010_BRpes.Rdata")
  beep()

  

# Clean Global Env.
rm(list=ls())
gc(reset = T)

beep() # beep alert


