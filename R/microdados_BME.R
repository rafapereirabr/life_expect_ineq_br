

# Load data
# 
# mort <- fread("R:/Dropbox/bases_de_dados/censo_demografico/censo_2010antigo/dados_csv2010/censo2010_BRmor.csv",
# select = myvariblesMOR)
# 
# pes <- fread("R:/Dropbox/bases_de_dados/censo_demografico/censo_2010antigo/dados_csv2010/censo2010_BRpes.csv",
# select = myvariblesPES)

censo2010_BRdom <- fread("R:/Dropbox/bases_de_dados/censo_demografico/censo_2010antigo/dados_csv2010/censo2010_BRdom.csv"),
select = myvariblesDOM)

gc(reset = T)


# qual proporcao de dom fazios imputados ???
table(censo2010_BRdom$V4001)





# remove missing info in income variable
censo2010_BRdom[, HIPC := V6531]
summary(censo2010_BRdom$HIPC)### >>>>>>>>>>>> 64426 NA's >????????????

# remove households with missing info of HIPC (64426 cases, 1.04% of all households
censo2010_BRdom2 <- subset(censo2010_BRdom, !is.na(HIPC))

#   gera quintis

quintile_br2 <- wtd.quantile(censo2010_BRdom2$HIPC, weights=censo2010_BRdom$V0010, probs=seq(0, 1, by=0.2), type='quantile', normwt=T, na.rm=TRUE)
quintile_br2




  

# read tables from BME ----------------------

pop_table2010 <- read_xlsx(sheet="populacao", path="R:/Dropbox/life expectancy (inequality)/income/ineq_LifeExp shared/input_tables/obitos_quintil_renda.xlsx")
deaths_table2010 <- read_xlsx(sheet="obitos", path="R:/Dropbox/life expectancy (inequality)/income/ineq_LifeExp shared/input_tables/obitos_quintil_renda.xlsx")

setDT(pop_table2010)
setDT(deaths_table2010)





##### POP ------------------


# soma pop por sexo, idade, renda
pop_table2010 <- pop_table2010[, .(pop2=sum(pop2)), by=.(sex,age,quintileBR)]
head(pop_table2010)

deaths_table2010 <- deaths_table2010[, .(deaths=sum(deaths)), by=.(sex,age,quintileBR)]
head(deaths_table2010)

# remove death data with no age
deaths_table2010 <- subset(deaths_table2010, age != "Ignorado")






### NATIONAL TABLE ----------------

# Format table
# create single national table to use DDM package85
national_table <- left_join(pop_table2010, deaths_table2010)   %>% setDT()


# add column with date of reference of the census
#national_table[, date2 := as.Date("31/07/2010", format="%d/%m/%Y") ]



# change ge categories

# agrega para 85+
national_table[ age== "De 85 a 89 anos", age :="85+"]
national_table[ age== "De 90 a 94 anos", age :="85+" ]
national_table[ age== "De 95 a 99 anos",  age :="85+"]
national_table[ age== "100 anos ou mais",  age :="85+"]
table(national_table$age)

# agrega pop e morte para 85+
national_table <- national_table[, .(pop2=sum(pop2), deaths=sum(deaths)), by=.(sex,age,quintileBR)]

19 * 2 * 5 # 19 idades, 2 sexos 5 rendas


# age85 groups
# national_table[, age :=  ifelse( age=="Menos de um ano", 0,
#                           ifelse( age=="De 1 a 4 anos", 1,
#                           ifelse( age=="De 5 a 9 anos", 5,
#                           ifelse( age=="De 10 a 14 anos", 10,
#                           ifelse( age=="De 15 a 19 anos", 15,
#                           ifelse( age=="De 20 a 24 anos", 20,
#                           ifelse( age=="De 25 a 29 anos", 25,
#                           ifelse( age=="De 30 a 34 anos", 30,
#                           ifelse( age=="De 35 a 39 anos", 35,
#                           ifelse( age=="De 40 a 44 anos", 40,
#                           ifelse( age=="De 45 a 49 anos", 45,
#                           ifelse( age=="De 50 a 54 anos", 50,
#                           ifelse( age=="De 55 a 59 anos", 55,
#                           ifelse( age=="De 60 a 64 anos", 60,
#                           ifelse( age=="De 65 a 69 anos", 65,
#                           ifelse( age=="De 70 a 74 anos", 70,
#                           ifelse( age=="De 75 a 79 anos", 75,
#                           ifelse( age=="De 80 a 84 anos", 80,
#                           ifelse( age=="85+", 85, 999999)
#                           ))))))))))))))))))]

table(national_table$age)


national_table[, age :=  ifelse( 
  age=="Menos de um ano", "0-1",
ifelse( age=="De 1 a 4 anos",   "1-4",
ifelse( age=="De 5 a 9 anos",   "5-9",
ifelse( age=="De 10 a 14 anos", "10-14",
ifelse( age=="De 15 a 19 anos", "15-19",
ifelse( age=="De 20 a 24 anos", "20-24",
ifelse( age=="De 25 a 29 anos", "25-29",
ifelse( age=="De 30 a 34 anos", "30-34",
ifelse( age=="De 35 a 39 anos", "35-39",
ifelse( age=="De 40 a 44 anos", "40-44",
ifelse( age=="De 45 a 49 anos", "45-49",
ifelse( age=="De 50 a 54 anos", "50-54",
ifelse( age=="De 55 a 59 anos", "55-59",
ifelse( age=="De 60 a 64 anos", "60-64",
ifelse( age=="De 65 a 69 anos", "65-69",
ifelse( age=="De 70 a 74 anos", "70-74",
ifelse( age=="De 75 a 79 anos", "75-79",
ifelse( age=="De 80 a 84 anos", "80-84",
ifelse( age=="85+",             "85+", 999999)
))))))))))))))))))]





table(national_table$age)


# Sex
national_table[, sex :=  ifelse( sex==1, "Men", "Women")]
table(national_table$sex)                                        
                                                
                                                
# income  groups
national_table[, quintileBR :=  ifelse( quintileBR=="Entre 0 e 170", 1,
                                ifelse( quintileBR=="Entre 170.01 e 340", 2,
                                ifelse( quintileBR=="Entre 340.01 e 522", 3,
                                ifelse( quintileBR=="Entre 522.01 e 1000", 4,
                                ifelse( quintileBR=="Mais de 1000", 5, NA)))))]

table(national_table$quintileBR)


# final edit before life table
head(national_table)


names(national_table) <- c('sex', 'age85', 'quintileBR', 'population', 'death_count')


# Reorder Factor Levels of age85 Groups
national_table[, quintileBR := factor(quintileBR, ordered = TRUE,levels = 1:5)]
national_table[, age85 := factor(age85, ordered = TRUE,
                                 levels = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                            "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                                            "70-74","75-79","80-84","85+"))]





root_table_nat <- copy(national_table)


############ Compute National Life Table QUINTILE ----------------------------------------------------------------



# Create new var nMx 'mortality rate'
root_table_nat[ , nMx := death_count / population ]

# create unique ID for each life table
root_table_nat[ , tableid := with(root_table_nat, paste0(sex, quintileBR))]

# convert income variable to numeric
root_table_nat$quintileBR <- as.numeric(root_table_nat$quintileBR)
head(root_table_nat)





### Apply Life Table function to NATIONAL data root_table using a  Loop -----------------------


# Get functions to  Compute Life Tables (National and state levels) 
source("./Rscripts/_life table functions_ineqlifeexp.R")


life_table_nat_quintile

# get population sub-groups ( possible combinations of Sex and income decile)
groups <- unique(root_table_nat$tableid)

# loop
for (i in groups){
  print(i)
  y <- root_table_nat[tableid==i,]
  if (i == groups[1]) { 
    LIFETABLE_nat_quint <- life_table_nat_quintile(x, y, y$nMx) }
  else # if it's not the 1st file, save it appending the rows to the previous file
  {
    temp <- life_table_nat_quintile(x, y, y$nMx)
    LIFETABLE_nat_quint <- rbind(LIFETABLE_nat_quint, temp)
  }
}

# Here is the National Life Table by sex and income decile
head(LIFETABLE_nat_quint)

# convert income variable to numeric
LIFETABLE_nat_quint$quintileBR <- as.numeric(LIFETABLE_nat_quint$quintileBR)

# Save National Life Table
fwrite( LIFETABLE_nat_quint , './results/LIFETABLE_nat_quint_BME.csv' )





######## load Life Tables ######## ------------------------------


LIFETABLE_nat_quint <- fread('./results/LIFETABLE_nat_quint_BME.csv')




