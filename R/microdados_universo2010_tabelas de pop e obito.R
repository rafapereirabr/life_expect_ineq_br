
install.packages('Hmisc')
install.packages('data.table')

library(Hmisc)
library(data.table)



################################ BASE DOMICILIOS ################################
#################################################################################
#################################################################################

# carregar a base 
  domicilios <- load(_____)

# converte para data.table
  setDT(domicilios)
  
# manter apenas as variaives que vamos usar Regiao, variaives chave do domicilio e renda domiciliar per capita (V0001,V0002,V0011,V0300,V1001, HIPC)
  domicilios <- domicilios[, .(V0001,V0002,V0011,V0300,V1001, HIPC)]
  
  #clean RAM memory
  gc(reset = T)
  
# Estimar intervalos de quantil de renda Nacional
  # "HIPC" eh o nome da variavel de renda domiciliar per capita 
  decile_br <- wtd.quantile(domicilios$HIPC,  probs=seq(0, 1, by=0.1), type='quantile', normwt=T, na.rm=TRUE)
  quintile_br <- wtd.quantile(domicilios$HIPC, weights=domicilios$V0010, probs=seq(0, 1, by=0.2), type='quantile', normwt=T, na.rm=TRUE)

  # reclassify obervations based on income intervals
    domicilios[ , decileBR :=  cut( HIPC , decile_br, include.lowest= TRUE, labels=1:10, na.rm=T ) ]
    domicilios[ , quintileBR :=  cut( HIPC , quintile_br, include.lowest= TRUE, labels=1:5, na.rm=T ) ]

  
# Esse trecho classifica domicilios por decil de renda para cada regiao seguindo a distribuicao de renda em cada regiao
  # Estimar intervalos de quantil de renda para cada regiao
        regions <- unique(domicilios$V1001)
        for (i in regions) {
          temp <- domicilios[ V1001 == i , .(V1001, HIPC, V0010)] 
          
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
          domicilios[ V1001== i, quintileRegion :=  as.numeric(cut( HIPC , quintile, include.lowest= TRUE, labels=1:5, na.rm=T )) ]}
        # check size of groups
        table(domicilios$quintileRegion, domicilios$V1001)
        domicilios[, .(sum(V0010, na.rm=T)), by=.(quintileRegion, V1001)]
        
        
        

      
  
  
  
  
  
  
  

################################ BASE PESSOAS ###################################
#################################################################################
#################################################################################

  
# carregar a base 
  pessoas <- load(_____)
  
# converte para data.table
  setDT(pessoas)
  
  
  # manter apenas as variaives que vamos usar Regiao, variaives chave do domicilio, sexo e idade (V0001,V0002,V0011,V0300,V1001, V0601, V6036)
  pessoas <- pessoas[, .(V0001,V0002,V0011,V0300,V1001, V6036)]
  
  #clean RAM memory
  gc(reset = T)
  
# Age groups 85+
  agebreaks85 <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,200)
  agelabels85 = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
  pessoas[, age85:= cut(V6036, breaks=agebreaks85, right=FALSE, labels = agelabels85) ]
  
  
# Puxa informacao de quantil de renda da base de domicilios para pessoas
# fazendo merge com base nas variaveis (V0001,V0002,V0011,V0300,V1001)
  pessoas[domicilios, on=.(V0001,V0002,V0011,V0300,V1001), c('decileBR', 'quintileBR', 'quintileRegion') := list(i.decileBR, i.quintileBR, i.quintileRegion) ]
  
  # convert quantiles to numeric
  pessoas[, decileBR := as.numeric(decileBR) ]
  pessoas[, quintileBR := as.numeric(quintileBR) ]
  pessoas[, quintileRegion := as.numeric(quintileRegion) ]

    
  
  
  
  
  
        
        


################################ BASE OBITOS ####################################
#################################################################################
#################################################################################

# carregar a base 
obitos <- load(_____)

# converte para data.table
setDT(obitos)
  
  
# manter apenas as variaives que vamos usar Regiao, variaives chave do domicilio, sexo e idade de obito (V0001,V0002,V0011,V0300,V1001, V0704, V7052, V7051)
obitos <- obitos[, .(V0001,V0002,V0011,V0300,V1001, V6036)]
  
  
# Incluir mortalidade infantil
  obitos <- obitos[ V7052 < 100, V7051 := 0]
    
# remover obitos com idade desconhecida
  obitos <- obitos[ V7051 < 999]
  
  
# clean RAM memory
  gc(reset = T)
  
  
  
# Age groups 85+
  obitos[, age85:= cut(V7051, breaks=agebreaks85, right=FALSE, labels = agelabels85) ]


# Puxa informacao de quantil de renda da base de domicilios para pessoas
# fazendo merge com base nas variaveis (V0001,V0002,V0011,V0300,V1001)
  obitos[domicilios, on=.(V0001,V0002,V0011,V0300,V1001), c('decileBR', 'quintileBR', 'quintileRegion') := list(i.decileBR, i.quintileBR, i.quintileRegion) ]

# convert quantiles to numeric
  obitos[, decileBR := as.numeric(decileBR) ]
  obitos[, quintileBR := as.numeric(quintileBR) ]
  obitos[, quintileRegion := as.numeric(quintileRegion) ]
  


# Remove base de pessoas e limpa memoria RAM
  rm(domicilios)
  gc(reset = T)
  
  
################################ Tabela de Mortalidade (NUMERADOR) ####################################
#######################################################################################################
#######################################################################################################

  
# Tabela do numero de obitos por regiao (V1001), sexo(V0704), decil de renda (decileBR), quintil de renda nacional e regional (quintileBR,quintileRegion) e idade (age85)  
  deaths_table2010 <- obitos[ , .(deaths= .N ), by=.(V1001,V0704,decileBR,quintileBR,quintileRegion,age85)]
  deaths_table2010 <- subset(deaths_table2010, deaths >0 )



# muda nome das colunas da tabela
  colnames(deaths_table2010) <- c("region", "sex", "decileBR", 'quintileBR', "quintileRegion", "age85", "deaths")
  
# add column with date of reference of the census
  deaths_table2010[, date2 := as.Date("31/07/2010", format="%d/%m/%Y") ]


# save pop_table2010
  fwrite(deaths_table2010, "./input_tables/deaths_table2010.csv")
  
  
  
  

################################ Tabela de Populacao (DENOMINADOR) ####################################
#######################################################################################################
#######################################################################################################
  
  
# Tabela do numero de pessoas por regiao (V1001), sexo(V0601), decil de renda (decileBR), quintil de renda nacional e regional (quintileBR,quintileRegion) e idade (age85)  
  pop_table2010 <- pessoas[ , .(pop2= .N ), by=.(V1001,V0601,decileBR,quintileBR,quintileRegion, age85)]
  pop_table2010 <- subset(pop_table2010, pop2 >0 )

# muda nome das colunas da tabela
  colnames(pop_table2010) <- c("region",  "sex", "decileBR", "quintileBR", "quintileRegion", "age85", "pop2")

# add column with date of reference of the census
  pop_table2010[, date2 := as.Date("31/07/2010", format="%d/%m/%Y") ]


# save pop_table2010
  fwrite(pop_table2010, "./input_tables/pop_table2010.csv")
  
  
  
  
