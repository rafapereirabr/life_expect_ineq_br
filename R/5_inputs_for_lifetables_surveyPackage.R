


############ set working Directory ------------------
setwd("R:/Dropbox/life expectancy (inequality)/income/ineq_LifeExp shared") # set working Directory



##################### Load package85s -------------------------------------------------------

source("./Rscripts/_LoadPackages.R")


##################### create folder  -------------------------------------------------------


# # Create folder to save input tables and results
# dir.create(file.path(".", "input_tables"))
# dir.create(file.path(".", "results"))






######## 2000 input popultion table  -----------------------------------

# Load Survey Design Object
load('./data/design_2000.Rdata')

# check general distribution of population by age85
plot ( svytable(~age85, design = design_2000) )

# Generate Population count tables, by Region, Sex, age85 and Income group  (approx. 2 minutes)
pop_table2000 <- svytable(~V1001+V0401+age85, design = design_2000)
pop_table2000 <- data.table(pop_table2000)



# Change name of variables
colnames(pop_table2000) <- c("region",  "sex", "age85", "pop1")
#colnames(pop_table2000) <- c("region", "state", "sex", "age85", "pop1")

# add column with date of reference of the census
pop_table2000[, date1 := as.Date("01/08/2000", format="%d/%m/%Y") ]

# save pop_table2000
fwrite(pop_table2000, "./input_tables/pop_table2000.csv")


# clean memory
  rm(design_2000)
  gc(reset=T)
  



######## 2010 input population table  -----------------------------------

# Load Survey Design Object
  load("./data/design_pes2010.Rdata")


# Generate Population count tables, by Region, State, Sex, age85 and Income group  (approx. 2 minutes)
  system.time(pop_table2010 <- svytable(~V1001+V0601+decileBR+quintileBR+quintileRegion+age85, design = design_pes2010))
  pop_table2010 <- data.table(pop_table2010)
  head(pop_table2010)
  gc(reset = T)


# Change name of variables
  colnames(pop_table2010) <- c("region",  "sex", "decileBR", "quintileBR", "quintileRegion", "age85", "pop2")

# add column with date of reference of the census
  pop_table2010[, date2 := as.Date("31/07/2010", format="%d/%m/%Y") ]

# save pop_table2010
  fwrite(pop_table2010, "./input_tables/pop_table2010.csv")

  
### plot
  # reorder factor levels
  pop_table2010[, decileBR := factor(decileBR, ordered = TRUE,levels = 1:10)]
  pop_table2010[, age85 := factor(age85, ordered = TRUE,
                               levels = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                          "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                                          "70-74","75-79","80-84","85+"))]
  
  # check general distribution of population by age85
    plot ( svytable(~age85, design = design_pes2010) ) # population
    # or
    ggplot( data=pop_table2010, aes(x=age85, y=pop2) ) + geom_bar(stat="identity")
  
  # by income
    ggplot( data=pop_table2010, aes(x=age85, y=pop2, fill =decileBR) ) +
      facet_wrap(.~decileBR, ncol = 5) +
      geom_bar(stat="identity")
    
    
# clean memory
  rm(design_pes2010)
  gc(reset=T)

  
  
######## 2010 input Deaths table  -----------------------------------

# # Load Survey Design Object
  load("./data/design_deaths2010.Rdata")

# check general distribution of  by age85
plot ( svytable(~age85, design = design_deaths2010) ) #

# Generate Population and Death count tables, by State, Sex, age85 and Income group
#system.time(deaths_table2010 <- svytable(~V1001+V0001+V0704+decileBR+quintileBR+quintileRegion+quintileState+age85, design = design_deaths2010))
system.time(deaths_table2010 <- svytable(~V1001+V0704+decileBR+quintileBR+quintileRegion+age85, design = design_deaths2010))

# conver to data table format
deaths_table2010 <- data.table(deaths_table2010)
head(deaths_table2010)

# Change name of variables
colnames(deaths_table2010) <- c("region", "sex", "decileBR", 'quintileBR', "quintileRegion", "age85", "deaths")

# add column with date of reference of the census
  deaths_table2010[, date2 := as.Date("31/07/2010", format="%d/%m/%Y") ]


# save pop_table2010
  fwrite(deaths_table2010, "./input_tables/deaths_table2010.csv")


# clean memory
  rm(design_deaths2010)
  gc(reset=T)


# ######## 2000 input popultion table  -----------------------------------
# 
# # Load dataset
# load("./data/censo2000_BR.Rdata")
# setDT(censo2000_BR)
# 
# # Generate Population count tables, by Region, State, Sex, age85 and Income group  (approx. 2 minutes)
# pop_table2000 <- censo2000_BR[, .(pop1=sum(PES_PESSOA, na.rm=T)), by=.(V1001,  V0401, age85) ]
# pop_table2000 <- subset(pop_table2000, pop1 >0 )
# head(pop_table2000)
# 
# 
# # clean memory
# rm(design_2000, censo2000_BR)
# gc(reset=T)
# 
# # Change name of variables
# colnames(pop_table2000) <- c("region",  "sex", "age85", "pop1")
# #colnames(pop_table2000) <- c("region", "state", "sex", "age85", "pop1")
# 
# # add column with date of reference of the census
# pop_table2000[, date1 := as.Date("01/08/2000", format="%d/%m/%Y") ]
# 
# # save pop_table2000
# fwrite(pop_table2000, "./input_tables/pop_table2000.csv")
# 

# ######## 2010 input population table  -----------------------------------
# #### Weighted, no Survey design
# 
# 
# # load data people 2010
#   load("./data/censo2010_BRpes.Rdata")
#   setDT(censo2010_BRpes)
# 
#   summary(censo2010_BRpes$decileBR )
#   table(censo2010_BRpes$decileBR )
#   
# # Generate Population count tables, by Region, State, Sex, age85 and Income group  (approx. 2 minutes)
#   pop_table2010 <- censo2010_BRpes[ , .(pop2=sum(V0010)), by=.(V1001,V0601,decileBR,quintileBR,quintileRegion, age85)]
#   pop_table2010 <- subset(pop_table2010, pop2 >0 )
#   
# # Change name of variables
#   colnames(pop_table2010) <- c("region",  "sex", "decileBR", "quintileBR", "quintileRegion", "age85", "pop2")
#   
# # add column with date of reference of the census
#   pop_table2010[, date2 := as.Date("31/07/2010", format="%d/%m/%Y") ]
#   rm(censo2010_BRpes)
#   gc(reset = T)
# 
#   # save pop_table2010
#   fwrite(pop_table2010, "./input_tables/pop_table2010.csv")
#   
#   
#   
# ######## 2010 input Deaths table  -----------------------------------
# #### Weighted, no Survey design
#   
#   load("./data/censo2010_BRdeaths.Rdata")
#   setDT(censo2010_BRdeaths)
#   deaths_table201022 <- censo2010_BRdeaths[ , .(deaths=sum(V0010)), by=.(V1001,V0704,decileBR,quintileBR,quintileRegion,age85)]
#   deaths_table2010 <- subset(deaths_table2010, deaths >0 )
#   
#   colnames(deaths_table2010) <- c("region", "sex", "decileBR", 'quintileBR', "quintileRegion", "age85", "deaths")
#   # add column with date of reference of the census
#   deaths_table2010[, date2 := as.Date("31/07/2010", format="%d/%m/%Y") ]
#   rm(censo2010_BRdeaths)
#   gc(reset = T)
# 
#  # save pop_table2010
#   fwrite(deaths_table2010, "./input_tables/deaths_table2010.csv")
#   
#   

    






######## Re-calculate Deaths  -----------------------------------
# through indirect  estimation of death registration coverage85, package85 DDM
# https://github.com/timriffe/AdultCoverage85

pop_table2000 <- fread("./input_tables/pop_table2000.csv")
pop_table2010 <- fread("./input_tables/pop_table2010.csv")
deaths_table2010 <- fread("./input_tables/deaths_table2010.csv")



# Get a national table with pop and deaths by Region 
  national_2000pop <- pop_table2000[, lapply(.SD, sum, na.rm=TRUE), by=c("region", "sex", "age85","date1"), .SDcols=c("pop1") ] 
  national_2010pop <- pop_table2010[, lapply(.SD, sum, na.rm=TRUE), by=c("region", "sex", "age85","date2"), .SDcols=c("pop2") ] 
  national_2010deaths <- deaths_table2010[, lapply(.SD, sum, na.rm=TRUE), by=c("region", "sex", "age85","date2"), .SDcols=c("deaths") ] 

#national_2000pop <- na.omit(national_2000pop)
# national_2000pop[, region := as.character(region)]
# national_2000pop[, sex := as.character(sex)]
# national_2000pop[, age85 := as.character(age85)]


# create single national table to use DDM package85
national_table <- left_join(national_2000pop, national_2010pop) %>% left_join(., national_2010deaths)   %>% setDT()

# no loss information ?
  sum(pop_table2000$pop1) == sum(national_table$pop1)
  sum(pop_table2010$pop2) == sum(national_table$pop2)
  sum(deaths_table2010$deaths) == sum(national_table$deaths)





# Format table

  names(national_table)[3] <- "age"
  
  # cod = region ~ sex
  national_table[, cod := paste0(region,"_", sex)]
  
  # sex male =1, female =2
  national_table[, sex := ifelse( sex== 1, "m", "f")]
  
  # reoder columns
  national_table <- national_table[, c('cod', 'pop1', 'pop2', 'deaths', 'age', 'sex', 'date1', 'date2')]
  
  # # date
  # national_table[, date1 := as.Date("01/08/2000", format="%d/%m/%Y") ]
  # national_table[, date2 := as.Date("31/07/2010", format="%d/%m/%Y") ]
  # class(national_table$date1)
  
  # age85 groups
  national_table[, age := ifelse( age=="0-1", 0,
                          ifelse( age=="1-4", 1,
                          ifelse( age=="5-9", 5,
                          ifelse( age=="10-14", 10,
                          ifelse( age=="15-19", 15,
                          ifelse( age=="20-24", 20,
                          ifelse( age=="25-29", 25,
                          ifelse( age=="30-34", 30,
                          ifelse( age=="35-39", 35,
                          ifelse( age=="40-44", 40,
                          ifelse( age=="45-49", 45,
                          ifelse( age=="50-54", 50,
                          ifelse( age=="55-59", 55,
                          ifelse( age=="60-64", 60,
                          ifelse( age=="65-69", 65,
                          ifelse( age=="70-74", 70,
                          ifelse( age=="75-79", 75,
                          ifelse( age=="80-84", 80,
                          ifelse( age=="85+", 85, 999999)
                          ))))))))))))))))))]

head(national_table)

# save input DDM national_table
fwrite(national_table, "./input_tables/DDM_input_national_table.csv")


# check quality of national table
  sum(national_table$pop2)   
  sum(national_table$deaths) 
  
# NATIONAL Gross mortality rate
  sum(national_table$deaths) / sum(national_table$pop2)  * 100 # this should be close to 0.539

  
  
####### APPLY DDM package85 ---------------

correction_factors <- ddm( national_table )
correction_factors

### Apply DDM correction to NATIONAL data root_table

    # separate region and sex
      setDT(correction_factors)
      correction_factors <- correction_factors[, .(cod, ggbseg)]
      correction_factors[, region :=  as.numeric( stri_sub(cod, 1,1)) ]
      correction_factors[, sex :=  as.numeric( stri_sub(cod, 3,3)) ]
      head(correction_factors)


    # ROOT TABLE : Merge everything into one big data set 
      root_table <- left_join(pop_table2010, deaths_table2010) %>% setDT()
      root_table <- root_table[ order(region, sex, decileBR, quintileBR, quintileRegion) ] # sort data
      head(root_table)
    
      # no loss information ?
      sum(pop_table2010$pop2) == sum(root_table$pop2)
      sum(deaths_table2010$deaths) == sum(root_table$deaths, na.rm = T)
      
      
    # Make correction of death count
      correction_factors[, region := as.integer(region)]
      correction_factors[, sex := as.integer(sex)]
      root_table <- left_join(root_table, correction_factors, by=c("region", "sex")) %>% setDT()
      root_table[, death_count := deaths * ggbseg]
      head(root_table)
      


############## cleand and edit root_table -------------------


### AGGREGATE to Regional level
  
  root_table <- root_table[, .(pop2=sum(pop2, na.rm=T), death_count=sum(death_count, na.rm=T)), 
                           by= .(region, sex, decileBR, quintileBR, quintileRegion,age85, date2)]
  
  
# Reorder Factor Levels of age85 Groups
root_table[, age85 := factor(age85, ordered = TRUE,
                             levels = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                        "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                                        "70-74","75-79","80-84","85+"))]

# Recode Region variable
root_table[, region := ifelse(region==1, "North",
                              ifelse(region==2, "Northeast",
                                     ifelse(region==3, "Southeast",
                                            ifelse(region==4, "South",
                                                   ifelse(region==5, "Midwest", "ERROR")))))]

# Recode Sex variable
root_table[, sex := ifelse(sex==1, "Men", 
                           ifelse(sex==2, "Women", "ERROR"))]

# Name population column
setnames(root_table, 'pop2', 'population')

# # Create new column nMx 'mortality rate'
#   root_table[ , nMx := death_count / Population ]

# Subset and reorder columns
root_table <- root_table[, c("region", "sex", "decileBR", "quintileBR", "quintileRegion", "age85", "population", "death_count")]
head(root_table)

# total population
sum(root_table$population)
# total deaths
sum(root_table$death_count, na.rm = T)

# NATIONAL Gross mortality rate
  sum(root_table$death_count) / sum(root_table$population)  * 100 # this should be close to 0.539



# Save root table
fwrite( root_table, './results/root_table.csv' )


# Clean Global Env.
rm(list=ls())
gc(reset = T)




