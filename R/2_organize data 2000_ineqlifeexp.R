


# This script Recode Variables --------------------------


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
setwd("R:/Dropbox/life expectancy (inequality)/income/ineq_LifeExp shared") # set working Directory

setwd("Z:/Desktop/ineq_LifeExp shared") # set working Directory


##################### Load packages -------------------------------------------------------

source("./Rscripts/_LoadPackages.R")





########  Load datasets   -----------------------

  load("./data/censo2000_BRpes.Rdata")
  load("./data/censo2000_BRdom.Rdata")

  # set as data.table
  setDT(censo2000_BRpes)
  setDT(censo2000_BRdom)

  gc(reset = T)
  
  
######## RECODE Household Dataset -----------------------
  
# Calculate household income per capita (HIPC)
  censo2000_BRdom[, HIPC := V7616 / V7100]
  summary(censo2000_BRdom$HIPC)



######## RECODE Person Dataset -----------------------
  
  # function to Create new FPC variable "FPC"   # Finite Population Correction
  censo2000_BRpes[, FPC:=sum(PES_PESSOA), by = AREAP] # new FPC variable - Finite Population Correction
  
  # Create Age groups with 5y intervals
  agebreaks85 <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,200)
  agelabels85 = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
  censo2000_BRpes[, age85 := cut(V4752, breaks=agebreaks85, right=FALSE, labels = agelabels85) ]
  
  
  agebreaks70 <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,200)
  agelabels70 = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70+")
  censo2000_BRpes[, age70 := cut(V4752, breaks=agebreaks70, right=FALSE, labels = agelabels70) ]
  
  
  # merge individuals and household data
  censo2000_BR <- left_join(censo2000_BRpes, censo2000_BRdom, by= c("V0102", "V0103", "V0300", "AREAP", "V1001", "V1006"))
  setDT(censo2000_BR)
  head(censo2000_BR)
  
  gc(reset = T)
  
  
  # Save datasets in the folder of the project
  save(censo2000_BR, file="./data/censo2000_BR.Rdata")
  
  
  # clean RAM memory
  rm(censo2000_BRdom, censo2000_BRpes)
  gc(reset=T)
  beep() # beep alert
  
  
  
  
  
  
  ######### SURVEY DESIGN  ----------------------------------------
  
  # load data
  load("./data/censo2000_BR.Rdata")
  
censo2000_BR <- censo2000_BR[!is.na(V0300), ] # remove on2  NA case in V0300, # Control variable

# keep only variables we'll use: state, sex, age, income, region, weight, etc.
censo2000_BR <- censo2000_BR[, .(V0102, V0300, AREAP, PES_PESSOA, FPC, V1001, V0401, age85, age70, HIPC)] #, decileBR, quintileBR, decileRegion, quintileRegion, Vcount)]

gc(reset=T)

# remove certainty units
options( survey.lonely.psu = "remove" )

Sys.time()
# creat DESIGN object ( between 8 and  10 hours to compute) 
system.time(  design_2000 <- svydesign(data = censo2000_BR,
                                      id = ~V0300, # Control variable
                                      strata = ~AREAP , #Strata (area de ponderacao)
                                      weights = ~PES_PESSOA, #  weight 
                                      fpc = ~ FPC, # Finite Population Correction
                                      nest = TRUE ) )

Sys.time()
# save the complex sample survey design object
save( design_2000 , file = './data/design_2000.Rdata' )
Sys.time()
#beep() # beep alert

#11pm
# Clean Global Env.
rm(list=ls())
gc(reset = T)

Sys.time()
end_2 <- Sys.time()
