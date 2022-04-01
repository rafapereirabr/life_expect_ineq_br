# fazer calculo com nao imputados \\\ toda info do dom inputada
  

# na var V4001 --- 02 - Domicílio particular permanente ocupado sem entrevista realizada

# This script Recode Variables --------------------------


############ set working Directory ------------------
setwd("R:/Dropbox/life expectancy (inequality)/income/ineq_LifeExp shared") # set working Directory



##################### Load packages -------------------------------------------------------

source("./Rscripts/_LoadPackages.R")






### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
########## Work on HOUSEHOLDS dataset ---------------------------


### Load HOUSEHOLD datasets
  load("./data/censo2010_BRdom.Rdata")
  setDT(censo2010_BRdom)

### recode variables: HOUSEHOLDS

# Create new FPC variable "FPC"   # Finite Population Correction
  censo2010_BRdom[, FPC := sum(V0010, na.rm = T), by = V0011] # new FPC variable - Finite Population Correction

  
# change var name of household income per capita
  censo2010_BRdom[, HIPC := V6531]
  summary(censo2010_BRdom$HIPC)### >>>>>>>>>>>> 64426 NA's >????????????
  
  # remove households with missing info of HIPC (64426 cases, 1.04% of all households
  censo2010_BRdom <- subset(censo2010_BRdom, !is.na(HIPC))
#?????????????????????????????????????????????????????????????????????????????????????????????????  
#?????????????????????????????????????????????????????????????????????????????????????????????????
  
  
# get Percentiles
  decile_br <- wtd.quantile(censo2010_BRdom$HIPC, weights=censo2010_BRdom$V0010, probs=seq(0, 1, by=0.1), type='quantile', normwt=T, na.rm=TRUE)
  quintile_br <- wtd.quantile(censo2010_BRdom$HIPC, weights=censo2010_BRdom$V0010, probs=seq(0, 1, by=0.2), type='quantile', normwt=T, na.rm=TRUE)

    
# reclassify obervations based on intervals
  censo2010_BRdom[ , decileBR :=  cut( HIPC , decile_br, include.lowest= TRUE, labels=1:10, na.rm=T ) ]
  censo2010_BRdom[ , quintileBR :=  cut( HIPC , quintile_br, include.lowest= TRUE, labels=1:5, na.rm=T ) ]
  
  
# Check Pop size in each quantile
  censo2010_BRdom[, .(sum(V0010, na.rm=T)), by=quintileBR] 
  censo2010_BRdom[, .(sum(V0010, na.rm=T)), by=decileBR]

  
  
  # get quintile by Region
  regions <- unique(censo2010_BRdom$V1001)
  for (i in regions) {
    temp <- censo2010_BRdom[ V1001 == i , .(V1001, HIPC, V0010)] 
    
    if (i == regions[1]) {
      quintile_region <- wtd.quantile(temp$HIPC, weights=temp$V0010, probs=seq(0, 1, by=0.2), type='quantile', normwt=T, na.rm=TRUE) }
    else # if it's not the 1st file, save it appending the rows to the previous file
    {
      temp <- wtd.quantile(temp$HIPC, weights=temp$V0010, probs=seq(0, 1, by=0.2), type='quantile', normwt=T, na.rm=TRUE)
      quintile_region <- rbind(quintile_region, temp)
    }
  }
  
  quintile_region <- data.table(quintile_region)
  quintile_region$V1001 <- regions
  
  
  # REGONAL level: Categorize households according to their quintile position
  for (i in c(quintile_region$V1001) ) { 
    quintile <- as.vector( quintile_region[V1001== i , c(1:6)] )
    censo2010_BRdom[ V1001== i, quintileRegion :=  as.numeric(cut( HIPC , quintile, include.lowest= TRUE, labels=1:5, na.rm=T )) ]}
  # check size of groups
  table(censo2010_BRdom$quintileRegion, censo2010_BRdom$V1001)
  censo2010_BRdom[, .(sum(V0010, na.rm=T)), by=.(quintileRegion, V1001)]
  
  
  
  
  

  # get quintile by STATE
  states <- unique(censo2010_BRdom$V0001)
  for (i in states) {
    temp <- censo2010_BRdom[ V0001 == i , .(V0001, HIPC, V0010)] 
    
    if (i == states[1]) {
      quintile_state <- wtd.quantile(temp$HIPC, weights=temp$V0010, probs=seq(0, 1, by=0.2), type='quantile', normwt=T, na.rm=TRUE) }
    else # if it's not the 1st file, save it appending the rows to the previous file
    {
      temp <- wtd.quantile(temp$HIPC, weights=temp$V0010, probs=seq(0, 1, by=0.2), type='quantile', normwt=T, na.rm=TRUE)
      quintile_state <- rbind(quintile_state, temp)
    }
  }
  
  quintile_state <- data.table(quintile_state)
  quintile_state$V0001 <- states
  
  
  # REGONAL level: Categorize individuals according to their quintile position
  for (i in c(quintile_state$V0001) ) { 
    quintile <- as.vector( quintile_state[V0001== i , c(1:6)] )
    censo2010_BRdom[ V0001== i, quintileState :=  as.numeric(cut( HIPC , quintile, include.lowest= TRUE, labels=1:5, na.rm=T )) ]}
  # check size of groups
  table(censo2010_BRdom$quintileState, censo2010_BRdom$V0001)
  

# Save datasets in the folder of the project
  save( censo2010_BRdom, file= "./data/censo2010_BRdom.Rdata")
  beep() # beep alert
  
# save income percentiles for future reference
  fwrite( data.table(decile_br) , "./data/percentiles_decile_br.csv" )
  fwrite( data.table(quintile_br), "./data/percentiles_quintile_br.csv" )
  fwrite(quintile_region, "./data/percentiles_quintile_region.csv" )
  fwrite(quintile_state, "./data/percentiles_quintile_state.csv" )


  



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
########## Work on INDIVIDUALS dataset ---------------------------

# Load Individuals dataset
  load("./data/censo2010_BRpes.Rdata")
  setDT(censo2010_BRpes)
  gc(reset = T)


  head(censo2010_BRpes)
  sum(censo2010_BRpes$V0010) %>% round() == 190755799

# recode variables: INDIVIDUALS

# Create new FPC variable "FPC"   # Finite Population Correction
  censo2010_BRpes[, FPC:=sum(V0010), by = V0011] # new FPC variable - Finite Population Correction

# Age groups ending in 75+
  agebreaks85 <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,200)
  agelabels85 = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
  censo2010_BRpes[, age85:= cut(V6036, breaks=agebreaks85, right=FALSE, labels = agelabels85) ]

# Age groups ending in 70+
  agebreaks70 <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,200)
  agelabels70 = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70+")
  censo2010_BRpes[, age70 := cut(V6036, breaks=agebreaks70, right=FALSE, labels = agelabels70) ]

# Household Income per Capita - HIPC
  censo2010_BRpes[, HIPC := V6531  ]
  gc(reset = T)
  gc(reset = T)
  
  
  
##
## Retrieve info Income Percentiles of Households from the household data set
   censo2010_BRpes[censo2010_BRdom, on=.(V0001,V0002,V0011,V0300,V1001), c('decileBR', 'quintileBR', 'quintileRegion', 'quintileState') := list(i.decileBR, i.quintileBR, i.quintileRegion, i.quintileState) ]
  
# convert quantiles to numeric
  censo2010_BRpes[, decileBR := as.numeric(decileBR) ]
  censo2010_BRpes[, quintileBR := as.numeric(quintileBR) ]
  censo2010_BRpes[, quintileRegion := as.numeric(quintileRegion) ]
  censo2010_BRpes[, quintileState := as.numeric(quintileState) ]

  summary(censo2010_BRpes$decileBR)
  summary(censo2010_BRpes$quintileBR)
  summary(censo2010_BRpes$quintileRegion)
  summary(censo2010_BRpes$quintileState)
  
  
  # remove individuals with no decile info. (70972 cases, 0.344% of total)
  censo2010_BRpes <- subset(censo2010_BRpes, !is.na(decileBR))
  
  #clean memory
  gc(reset = T)
  gc(reset = T)


# Save datasets in the folder of the project
  save( censo2010_BRpes, file= "./data/censo2010_BRpes.Rdata")
  beep() # beep alert





### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
####### Create Survey Design of Individuals ----------------------------------------
#load("./data/censo2010_BRpes.Rdata")
gc(reset=T)

setDT(censo2010_BRpes)

# # keep only variables we'll use: sex, age, income, region, weight, etc.
# censo2010_BRpes <- censo2010_BRpes[, .(V0300, V0011, V0010, FPC, V0001, V1001, V0601, age85, age70, decileBR, quintileBR, quintileRegion, quintileState, HIPC)]
# gc(reset=T)
# gc(reset=T)


# remove certainty units
options( survey.lonely.psu = "remove" )

Sys.time()
# PERSON DESIGN (~11 to 15 hours to compute)
system.time(  design_2010 <- svydesign(data = censo2010_BRpes,
                                       id = ~V0300, # Control variable
                                       strata = ~V0011 , #Strata (area de ponderacao)
                                       weights = ~V0010, #  weight
                                       fpc = ~ FPC, # Finite Population Correction
                                       nest = TRUE ) )


# save the complex sample survey design object
save( design_2010 , file = './data/design_2010.Rdata' )
beep() # beep alert

# clean RAM memory
rm(censo2010_BRpes)
gc(reset = T)

21:30
beep()



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
########## Work on DEATHS dataset ---------------------------

# Load deaths dataset
  load("./data/censo2010_BRdom.Rdata")

  

# Load deaths dataset
  load("./data/censo2010_BRdeaths.Rdata")
  setDT(censo2010_BRdeaths)
  gc(reset = T)

# recode variables: DEATHS
  
  # How mnay death records with unknown age?
    censo2010_BRdeaths[ V7052 == 99] %>% nrow() + censo2010_BRdeaths[ V7051 == 999] %>% nrow()
  
  # include infant mortality
    censo2010_BRdeaths <- censo2010_BRdeaths[ V7052 < 100, V7051 := 0]
    censo2010_BRdeaths <- censo2010_BRdeaths[ V7051 < 999] # remove deaths with unknown age
  
  
  # Create Age groups with 5y intervals
    censo2010_BRdeaths[, age85 := cut(V7051, breaks=agebreaks85, right=FALSE, labels = agelabels85) ]
    censo2010_BRdeaths[, age70 := cut(V7051, breaks=agebreaks70, right=FALSE, labels = agelabels70) ]
    
  # function to Create new FPC variable "FPC"   # Finite Population Correction
    censo2010_BRdeaths[, FPC:=sum(V0010), by = V0011] # new FPC variable - Finite Population Correction
  
    
##
## Retrieve info Income Percentiles of Households from the household data set
  censo2010_BRdeaths[censo2010_BRdom, on=.(V0001,V0002,V0011,V0300,V1001), c('decileBR', 'quintileBR', 'quintileRegion', 'quintileState') := list(i.decileBR, i.quintileBR, i.quintileRegion, i.quintileState) ]

# convert quantiles to numeric
  censo2010_BRdeaths[, decileBR := as.numeric(decileBR) ]
  censo2010_BRdeaths[, quintileBR := as.numeric(quintileBR) ]
  censo2010_BRdeaths[, quintileRegion := as.numeric(quintileRegion) ]
  censo2010_BRdeaths[, quintileState := as.numeric(quintileState) ]
    
  summary(censo2010_BRdeaths$decileBR)
  summary(censo2010_BRdeaths$quintileBR)
  summary(censo2010_BRdeaths$quintileRegion)
  summary(censo2010_BRdeaths$quintileState)
  

#clean memory
gc(reset = T)
gc(reset = T)

    

# Save datasets in the folder of the project
  save( censo2010_BRdeaths, file= "./data/censo2010_BRdeaths.Rdata")
  beep() # beep alert


# general info of death records: Number o mussing values
  sum(is.na(censo2010_BRdeaths$age85)) # 0 cases of missing info on age
  sum(is.na(censo2010_BRdeaths$V0704)) # 0 cases of missing info on sex
  sum(is.na(censo2010_BRdeaths$HIPC)) # 0 cases of missing info 
  sum(is.na(censo2010_BRdeaths$decileBR)) # 0 cases of missing 
  sum(is.na(censo2010_BRdeaths$quintileState)) # 0 cases of missing 



# Clean Global Env.
#rm(list=setdiff(ls(), c("censo2010_BRpes", "censo2010_BRdeaths")))
gc(reset = T)






### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
####### Create Survey Design of Deaths ( 8 minutes) ----------------------------------------

#load("./data/censo2010_BRdeaths.Rdata")
setDT(censo2010_BRdeaths)

# keep only variables we'll use: sex, age, income, region, state, weight, etc.
censo2010_BRdeaths <- censo2010_BRdeaths[, .(V0300, V0001, V0011, V0010, FPC, V1001, V0704, age85, age70, decileBR, quintileBR, quintileRegion, quintileState, HIPC)]

# remove certainty units
options( survey.lonely.psu = "remove" )

# Create Survey DESIGN of Deaths data set  
system.time(    design_deaths2010 <- svydesign(data = censo2010_BRdeaths,
                                               id = ~V0300, # Control variable
                                               strata = ~V0011 , #Strata (area de ponderacao)
                                               weights = ~V0010, #  weight 
                                               fpc = ~ FPC, # Finite Population Correction
                                               nest = TRUE))

# save the complex sample survey design object
save( design_deaths2010 , file = './data/design_deaths2010.Rdata' )
beep()

# Clean Global Env.
rm(list=setdiff(ls(), c("design_deaths2010")))
gc(reset = T)
gc(reset = T)


