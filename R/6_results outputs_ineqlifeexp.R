GAP plots com texto

https://www.r-bloggers.com/ggplot2-exercising-with-ggalt-dumbbells/
http://data-steve.github.io/making-ggdumbbell-smarter/
http://stackoverflow.com/questions/40011005/adding-a-legend-to-a-dumbbell-chart-in-r
https://twitter.com/ken_mke/status/657539344929071104



Zoom ploT ??? library(ggforce)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
############ set working Directory
setwd("R:/Dropbox/life expectancy (inequality)/income/ineq_LifeExp shared") # set working Directory




##################### Load packages -------------------------------------------------------

source("./Rscripts/_LoadPackages.R")






############ Compute National Life Table DECILE ----------------------------------------------------------------

# load data
  root_table <- fread('./results/root_table.csv')

  
# Reorder Factor Levels of age85 Groups
  root_table[, age85 := factor(age85, ordered = TRUE,
                               levels = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                          "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                                          "70-74","75-79","80-84","85+"))]

  root_table[, decileBR := factor(decileBR, ordered = TRUE,levels = 1:10)]
  root_table[, quintileBR := factor(quintileBR, ordered = TRUE,levels = 1:5)]
  root_table[, quintileRegion := factor(quintileRegion, ordered = TRUE,levels = 1:5)]


# Collapse to get National info
  root_table_nat <- root_table[, lapply(.SD, sum, na.rm=TRUE), by=c("sex", "decileBR", "age85"), .SDcols=c("population", "death_count") ] 
  root_table_nat <- na.omit(root_table_nat)

# Create new var nMx 'mortality rate'
  root_table_nat[ , nMx := death_count / population ]

# create unique ID for each life table
  root_table_nat[ , tableid := with(root_table_nat, paste0(sex, decileBR))]

# convert income variable to numeric
  root_table_nat$decileBR <- as.numeric(root_table_nat$decileBR)
  head(root_table_nat)

length( unique(root_table_nat$tableid)) ==20

# Get functions to  Compute Life Tables (National and state levels) 
  source("./Rscripts/_life table functions_ineqlifeexp.R")


### Apply Life Table function to NATIONAL data root_table using a  Loop
  life_table_nat_decile
  life_table_nat_quintile()
  # get population sub-groups ( possible combinations of Sex and income decile)
    groups <- unique(root_table_nat$tableid)
  
  # loop
    for (i in groups){
      print(i)
      y <- root_table_nat[tableid==i,]
      if (i == groups[1]) { 
        LIFETABLE_nat_quint <- life_table_nat_decile(x, y, y$nMx) }
      else # if it's not the 1st file, save it appending the rows to the previous file
      {
        temp <- life_table_nat_decile(x, y, y$nMx)
        LIFETABLE_nat_quint <- rbind(LIFETABLE_nat_quint, temp)
      }
    }

# Here is the National Life Table by sex and income decile
  head(LIFETABLE_nat_quint)

# convert income variable to numeric
  LIFETABLE_nat_quint$decileBR <- as.numeric(LIFETABLE_nat_quint$decileBR)

# Save National Life Table
  fwrite( LIFETABLE_nat_quint , './results/LIFETABLE_nat_quint.csv' )



  
  
  


############ Compute National Life Table QUINTILE ----------------------------------------------------------------

# Collapse to get National info
  root_table_nat <- root_table[, lapply(.SD, sum, na.rm=TRUE), by=c("sex", "quintileBR", "age85"), .SDcols=c("population", "death_count") ] 
  root_table_nat <- na.omit(root_table_nat)

# Create new var nMx 'mortality rate'
  root_table_nat[ , nMx := death_count / population ]

# create unique ID for each life table
  root_table_nat[ , tableid := with(root_table_nat, paste0(sex, quintileBR))]

# convert income variable to numeric
  root_table_nat$quintileBR <- as.numeric(root_table_nat$quintileBR)
  head(root_table_nat)


### Apply Life Table function to NATIONAL data root_table using a  Loop
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
  fwrite( LIFETABLE_nat_quint , './results/LIFETABLE_nat_quint.csv' )

  
  
  
  
############ Compute REGIONAL Life Tables ----------------------------------------------------------------
  
### Apply life table function to REGIONAL data root_table using a  Loop
  
# 1st, Collapse root_table to get Regional info
  root_table_reg <- root_table[, lapply(.SD, sum, na.rm=TRUE), by=c("region", "sex", "quintileRegion", "age85"), .SDcols=c("population", "death_count") ] 
  root_table_reg <- root_table_reg[order(region, sex, quintileRegion, age85)]
  root_table_reg <- na.omit(root_table_reg)
  
# Create new var nMx 'mortality rate'
  root_table_reg[ , nMx := death_count / population ]

# create unique ID for each life table
  root_table_reg[ , tableid := with(root_table_reg, paste0(region, sex, quintileRegion))]



### Apply function

# get population sub-groups ( possible combinations of Sex, region and income quintile)
  groups <- unique(root_table_reg$tableid)
  
  # loop
  for (i in groups){
    print(i)
    y <- root_table_reg[tableid==i,]
    if (i == groups[1]) { 
      LIFETABLE_reg_quint <- life_table_region_quintile(x, y, y$nMx) }
    else # if it's not the 1st file, save it appending the rows to the previous file
    {
      temp <- life_table_region_quintile(x, y, y$nMx)
      LIFETABLE_reg_quint <- rbind(LIFETABLE_reg_quint, temp)
    }
  }
  

# Here is the REGIONAL Life Table by sex and income decile
  head(LIFETABLE_reg_quint)

# convert income variable to numeric
  LIFETABLE_reg_quint$quintileRegion <- as.numeric(LIFETABLE_reg_quint$quintileRegion)

# Save Regional Life Tables
  fwrite( LIFETABLE_reg_quint , './results/LIFETABLE_reg_quint.csv' )
  
# Clean Global Env.
rm(list=ls())
gc(reset = T)










######## load Life Tables ######## ------------------------------

LIFETABLE_nat_quint <- fread('./results/LIFETABLE_nat_quint.csv')
LIFETABLE_nat_quint <- fread('./results/LIFETABLE_nat_quint.csv')
LIFETABLE_reg_quint <- fread('./results/LIFETABLE_reg_quint.csv')


LIFETABLE_nat_quint[, decileBR := factor(decileBR, ordered = TRUE,levels = 1:10)]


  # add labels to income variable
    LIFETABLE_nat_quint[, decile := decileBR]
    LIFETABLE_nat_quint[, decileBR := factor(decileBR, levels=c(1:10), labels = c("D1 - poorest 10%","D2","D3","D4",'D5','D6','D7','D8','D9', "D10 - richest 10%"))]
    
    LIFETABLE_nat_quint[, quintileBR := factor(quintileBR, levels=c(1:5), labels = c("Q1 - poorest 20%","Q2","Q3","Q4","Q5 - richest 20%"))]
    LIFETABLE_reg_quint[, quintileRegion := factor(quintileRegion, levels=c(1:5), labels = c("Q1 - poorest 20%","Q2","Q3","Q4","Q5 - richest 20%"))]

      

    # calculate expected Age to live
    LIFETABLE_nat_quint[, longevity := x +ex ]
    LIFETABLE_nat_quint[, longevity := x +ex ]
    LIFETABLE_reg_quint[, longevity := x +ex ]
    

    LIFETABLE_nat_quint[, px := 1 - nqx]
    LIFETABLE_nat_quint[, sx := px * shift(px, 1), by=tableid]
    
    
    

        
    
    # Subset Ages terminating in 0
    temp <- LIFETABLE_nat_quint[x %in% c(0,10,20,30,40,50,60,70,85)]

        


######## CHARTS CHARTS  CHARTS  CHARTS  CHARTS######## 



# # Interpolate color for plotting
#   # check display.brewer.all() for other avilable color RampPalettes
#   colourCount = 10
#   getPalette = colorRampPalette(brewer.pal(9, "Blues"))

# Create Basic Aesthetic stantard for the plots
   baseplot <- theme_minimal() +
               theme( axis.text  = element_text(face="bold"),
                      strip.text = element_text(size = 11, face ="bold"),
                      legend.text = element_text(size = 11) )





   


######## Fig.2 Life Expectancy gap at different ages by sex --------------

fig2a <- ggplot(data=subset(LIFETABLE_nat_quint, decileBR %like% 'D1'), aes(y=factor(x), x=longevity)) + 
            geom_path(aes(x=longevity, y=factor(x), group=x), color="gray70") +
            geom_point(aes(color=factor(decileBR)), size=4 ) + 
            facet_grid(sex ~ .) + # vertical
            baseplot + 
            scale_x_continuous(name="Life expectancy", limits=c(60, 100)) + #expand=c(0,0), 
            scale_y_discrete(name="Age") +
            scale_color_discrete(name="Income Decile") +
            theme(legend.position = "top") + 
            theme(panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank())


ggsave(fig2a, file="./plots/fig_2a_dot2.png", dpi = 800,
       width = 20, height = 15, units = "cm")




fig2b <-   ggplot() +
              geom_line(data=subset(LIFETABLE_nat_quint, decileBR %like% 'D1'), 
                        aes(x=factor(x), y=longevity, color=decileBR, group=decileBR), size=1) +
              baseplot +
              facet_wrap(~sex) +
              scale_color_discrete(name="Income Decile") +
              scale_y_continuous(name="Life Expectancy", limits=c(60, 100)) + 
              theme(legend.position = "top") +
              scale_x_discrete(name="Age") 


ggsave(fig2b, file="./plots/fig_2b_line2.png", dpi = 800,
       width = 20, height = 10, units = "cm")


          # all Deciles in gray scale
fig2c <-  ggplot(data=LIFETABLE_nat_quint)+
                geom_line(aes(x=factor(x), y=longevity, color=decileBR, group=decileBR), size=1) +
                baseplot +
                facet_wrap(~sex) +
                scale_y_continuous(name="Life Expectancy", limits=c(60, 100)) + 
                scale_x_discrete(name="Age") +
                theme(legend.position = "top") +
                scale_colour_grey( start = .8, end = 0, guide = guide_legend(title = "Income Decile")) 

ggsave(fig2c, file="./plots/fig_2c_line_all_p.png", dpi = 800,
       width = 20, height = 10, units = "cm")

fig2d <- ggplot(data=LIFETABLE_nat_quint)+
            geom_line(aes(x=factor(x), y=longevity, color=quintileBR, group=quintileBR), size=1) +
            baseplot +
            facet_wrap(~sex) +
            scale_y_continuous(name="Life Expectancy", limits=c(60, 100)) + 
            scale_x_discrete(name="Age") +
            theme(legend.position = "top") +
            scale_colour_grey( start = .8, end = 0, guide = guide_legend(title = "Income Quintile")) 

ggsave(fig2d, file="./plots/fig_2d_line_allq_BME.png", dpi = 800,
       width = 20, height = 10, units = "cm")
              
              
          # Gender conversion of Ex in every income level
              ggplot(data=LIFETABLE_nat_quint)+
                geom_line(aes(x=factor(x), y=longevity, color=sex, group=sex)) +
                baseplot +
                facet_wrap(~decileBR) +
                scale_y_continuous(name="Life Expectancy", limits=c(60, 100)) + 
                scale_x_discrete(name="Age") 
              
              ggplot()+
                geom_line(data=subset(LIFETABLE_nat_quint, decileBR %like% 'D1'), aes(x=factor(x), y=longevity, color=sex, group=sex), size=1) +
                baseplot +
                facet_wrap(~decileBR) +
                scale_y_continuous(name="Life Expectancy", limits=c(60, 100)) + 
                scale_x_discrete(name="Age") 

# Ver 
https://matthewdharris.com/2016/08/12/ggplot2-step-by-step/
https://drsimonj.svbtle.com/plotting-background-data-for-groups-with-ggplot2?utm_content=buffer1e54a&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer



# NATIONAL Difference between richest and poorest men
  Ex0_richest_men <- LIFETABLE_nat_quint[ x=='0' & sex== 'Men' & decileBR %like% 'richest', .(ex) ]
  Ex0_poorest_men <- LIFETABLE_nat_quint[ x=='0' & sex== 'Men' & decileBR %like% 'poorest', .(ex) ]
  Ex0_richest_men - Ex0_poorest_men

    LIFETABLE_nat_quint[ x=='0' & sex== 'Men', .(decileBR, ex) ][ex==max(ex)]
    LIFETABLE_nat_quint[ x=='0' & sex== 'Men', .(decileBR, ex) ][ex==min(ex)]
  
# NATIONAL Difference between richest and poorest women
  Ex0_richest_women <- LIFETABLE_nat_quint[ x=='0' & sex== 'Women' & decileBR %like% 'richest', .(ex) ]
  Ex0_poorest_women <- LIFETABLE_nat_quint[ x=='0' & sex== 'Women' & decileBR %like% 'poorest', .(ex) ]
  Ex0_richest_women - Ex0_poorest_women
  
    LIFETABLE_nat_quint[ x=='0' & sex== 'Women', .(decileBR, ex) ][ex==max(ex)]
    LIFETABLE_nat_quint[ x=='0' & sex== 'Women', .(decileBR, ex) ][ex==min(ex)]

  
  
  
######## Fig.3 Life Expectancy at birth by region, sex and income -----
  
  # subset poorest and richest groups at age 0
  LIFETABLE_reg_subset <- LIFETABLE_reg_quint[ (quintileRegion %like% 'poorest' |
                                            quintileRegion %like% 'richest') & x == 0, ]

  

  
  # Reorder values
  LIFETABLE_reg_subset$region <- reorder(LIFETABLE_reg_subset$region, LIFETABLE_reg_subset$ex)
  LIFETABLE_reg_quint$region <- reorder(LIFETABLE_reg_quint$region, LIFETABLE_reg_quint$ex)
  

  # simples
  fig3a <- ggplot(data=LIFETABLE_reg_subset, aes(y=region, x=ex)) + 
            geom_path(aes(x=ex, y=region, group=region), color="gray70") +
            geom_point(aes(color=factor(quintileRegion)), size=4 ) + 
            facet_grid(sex ~ .) + # vertical
            baseplot + 
            scale_x_continuous(name="Life expectancy at birth", limits=c(65,85)) + 
            scale_y_discrete(name="Region") +
            scale_color_discrete(name="Income Quintile") +
            theme(legend.position="top") 
    

  
                  
  fig3b_vertical <- ggplot(data=subset(LIFETABLE_reg_quint, x==0), aes(y=ex, x=region)) +
                        geom_point(aes(color=factor(quintileRegion)), size=4 ) +
                        facet_grid(.~sex ) + # vertical
                        baseplot +
                        theme(legend.position="top") +
                        scale_y_continuous(name="Life expectancy at birth", limits=c(65, 85)) +
                        scale_x_discrete(name="Region") +
                        #scale_color_brewer( palette="Oranges", name = "Income Quintile")
                        scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) # Gay scale
  
  
  # fig3b_horizontal <- ggplot(data=subset(LIFETABLE_reg_quint, x==0), aes(y=region, x=ex)) + 
  #                         geom_point(aes(color=factor(quintileRegion)), size=4 ) + 
  #                         facet_grid(sex ~ .) + # vertical
  #                         baseplot + 
  #                         scale_x_continuous(name="Life expectancy at birth", limits=c(65,85)) + 
  #                         scale_y_discrete(name="Region") +
  #                         theme(legend.position="top") +
  #                         scale_color_brewer( palette="Oranges", name = "Income Quintile")
  #                         scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) # Gay scale
  
  
# save Plot
  ggsave(fig3a, file="./plots/fig_3a_simples2.png", dpi = 800,  width = 6.5)

  ggsave(fig3b_vertical, file="./plots/fig3b_vertical22.png", dpi = 800, width = 9, height = 5)
         
#  ggsave(fig3b_horizontal, file="./plots/fig3b_horizontal.png", dpi = 800, width = 7)
  
  
# REGIONAL DifferenceS between richest and poorest women [LIFE EXPECT AT BIRTH]
  
  table5 <- LIFETABLE_reg_subset[x==0, .(region,sex,quintileRegion,ex)]
  
  max(table5$ex) -   min(table5$ex)
  
  table5[ ex == max(ex) ]
  table5[ ex == min(ex) ]
  
  

# Size of gap whithin each region
max(table5$ex[which(table5$region =="Southeast")]) -
min(table5$ex[which(table5$region =="Southeast")])

max(table5$ex[which(table5$region =="South")]) -
min(table5$ex[which(table5$region =="South")])

max(table5$ex[which(table5$region =="Central-West")]) -
min(table5$ex[which(table5$region =="Central-West")])

max(table5$ex[which(table5$region =="North")]) -
min(table5$ex[which(table5$region =="North")])

max(table5$ex[which(table5$region =="Northeast")]) -
min(table5$ex[which(table5$region =="Northeast")])



# clean memory
gc(reset = T)

######## Fig.4 Compare regional Probability of death -----

fig4 <- 
  ggplot(LIFETABLE_reg_quint, aes(x = factor(x), y = log(nqx), group=quintileRegion, color = factor(quintileRegion))) +
  geom_line() +
  facet_grid(region ~ sex) +
  baseplot +
  theme(legend.position="top") +
  scale_x_discrete(name="Age") +
  # scale_color_brewer( palette="Oranges", name = "Income Quintile")
  scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) # Gay scale



ggsave(fig4, file="./plots/fig4_nqx_region.png", dpi = 800,
       width = 20, height = 25, units = "cm")

fig4_national <- 
ggplot(LIFETABLE_nat_quint, aes(x = factor(x), y = log(nqx), group=decileBR, color = factor(decileBR))) +
  geom_line() +
  #geom_point(aes(color=factor(decileBR)), size=1 ) + 
  facet_grid(. ~ sex) +
  baseplot +
  theme(legend.position="top") +
  scale_x_discrete(name="Age") +
 # scale_color_brewer( palette="Oranges", name = "Income Quintile")
  scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) # Gay scale


ggsave(fig4_national, file="./plots/fig4_nqx_brazil_p.png", dpi = 800,
       width = 25, height = 12, units = "cm")


fig4_nationalquin <- 
ggplot(LIFETABLE_nat_quint, aes(x = factor(x), y = log(nqx), group=quintileBR, color = factor(quintileBR))) +
  geom_line() +
  #geom_point(aes(color=factor(quintileBR)), size=1 ) + 
  facet_grid(. ~ sex) +
  baseplot +
  theme(legend.position="top") +
  scale_x_discrete(name="Age") +
  # scale_color_brewer( palette="Oranges", name = "Income Quintile")
  scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) # Gay scale


ggsave(fig4_nationalquin, file="./plots/fig4_nqx_brazil_quin_BME.png", dpi = 800,
       width = 25, height = 12, units = "cm")


######## Fig.5 Compare regional life expectancy -----

  fig5 <- 
    ggplot(LIFETABLE_reg_quint, aes(x = factor(x), y = longevity, group=quintileRegion, color = factor(quintileRegion))) +
    geom_line(size= .8) +
    facet_grid(region ~ sex) +
    baseplot +
    theme(legend.position="top") +
    scale_y_continuous(name="Life Expectancy", limits=c(60, 100)) + 
    scale_x_discrete(name="Age") +
    # scale_color_brewer( palette="Oranges", name = "Income Quintile")
    scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) # Gay scale
  
  
  
  ggsave(fig5, file="./plots/fig5_Ex_region.png", dpi = 800,
         width = 20, height = 20, units = "cm")








  
########## fig.Extra Life Expectancy at different ages sex and income -----
  

#### Px 
fig_px <- 
  ggplot(data=subset(LIFETABLE_nat_quint,  x <65), aes(x=x, y= px )) + 
  geom_point(aes(color=factor(decileBR)), size=4 ) + 
  facet_grid(.~sex) + # vertical
  baseplot +
  scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) +
  theme(legend.position="top")


ggsave(fig_px, file="./plots/zextra_fig_px.png", dpi = 800,
       width = 30, height = 15, units = "cm")



# nQx
fig_nqx_extremes <- 
  ggplot(data=subset(LIFETABLE_nat_quint,  decileBR %like% 'D1'), aes(x=factor(x), y=log(nqx))) + 
  geom_path(aes(y=log(nqx), x=factor(x), group=x), color="gray70") +
  geom_point(aes(color= decileBR ), size=4 ) + 
  facet_grid(. ~sex) + # vertical
  theme(legend.position = "top") +
  scale_x_discrete(name="Age") +
  
  baseplot +
  theme(legend.position="top")

ggsave(fig_nqx_extremes, file="./plots/fig_nqx_extremes.png", dpi = 800,
       width = 25, height = 12, units = "cm")


fig_nqx_all <- 
  ggplot(data=subset(LIFETABLE_nat_quint), aes(x=x, y= log(nqx) )) + 
  #geom_path(aes(x= x, y= log(nqx), group=x), color="gray70") +
  geom_point(aes(color=factor(decileBR)), size=4 ) + 
  facet_grid(.~sex) + # vertical
  baseplot +
  scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) +
  theme(legend.position="top")

ggsave(fig_nqx_all, file="./plots/zextra_fig_nqx_all.png", dpi = 800,
       width = 30, height = 20, units = "cm")






fig999_extra <- 
    ggplot(data=temp)+
    geom_line(aes(x=factor(decileBR), y=longevity, color=sex, group=sex)) +
    baseplot +
    facet_wrap(~x) +
    scale_y_continuous(name="Life Expectancy at age") + 
    scale_x_discrete(name="Income decile") +
    scale_color_discrete(name="Sex") +
    theme(legend.position="top")
  
  
  ggsave(fig999_extra, file="./plots/extra_fig999_extra.png", dpi = 800,
         width = 33, height = 20, units = "cm")
  
  
 
  
  
  
  
  # failed
  ggplot(data=subset(LIFETABLE_reg_quint, x==0))+
    geom_line(aes(x= quintileRegion, y=longevity, color=region, group=region), size=1) +
    baseplot +
    facet_wrap(~sex) +
    scale_y_continuous(name="Life Expectancy", limits=c(60, 100)) + 
    scale_x_discrete(name="Age") +
    theme(legend.position = "top") +
    scale_colour_grey( start = .8, end = 0, guide = guide_legend(title = "Income Quintile")) 
  
  
  
  
  
  # # graf_1 National Life Expectancy by Sex and Income Level in 2010
  #   
  #   graf1 <- ggplot(LIFETABLE_nat, aes(x = x, y = ex, group=decileBR, color = factor(decileBR))) +
  #                   geom_line() +
  #                   facet_wrap(~sex) + 
  #                   scale_colour_manual(values = getPalette(colourCount)) +
  #                   baseplot
  #   
  #   # create column 'Remaining Years of Life' 
  #     LIFETABLE_nat$remaining <- LIFETABLE_nat$x + LIFETABLE_nat$ex
  #   #plot
  #     ggplot(LIFETABLE_nat, aes(x = x, y = remaining, group=decileBR, color = factor(decileBR))) +
  #       geom_line() +
  #       facet_wrap(~sex) + 
  #       scale_colour_manual(values = getPalette(colourCount)) +
  #       baseplot
  #   
  # 
  #   # save Plot
  #     ggsave(graf1, file="graf0 National LifeEx.png" )
  
  
  
  
  #table_nat <- LIFETABLE_nat[ x ==0 & (decileBR %like% 'rich' | decileBR %like% 'poor') , .(tableid, sex, decileBR, x, ex)]
  
  
  # # graf_2 Compare Probability of death by Sex and Income Level in 2010
  # graf2 <- ggplot(LIFETABLE_nat, aes(x = x, y = log(nqx), group=decileBR, color = factor(decileBR))) +
  #                 geom_line(data = LIFETABLE_nat) +
  #                 facet_wrap(~sex) +
  #                 scale_colour_manual(values = getPalette(colourCount), name="Incomde Deciles") +
  #                 baseplot
  # 
  # # save Plot
  # ggsave(graf2, file="graf2 National death probability.png" )
  
  
  
  
 
  
  
  # graf_3 Regional Expectancy by Sex and Income Level in 2010
    # graf3 <- ggplot(LIFETABLE_reg, aes(x = x, y = longevity, group=quintileRegion, color = factor(quintileRegion))) +
    #                 geom_line() +
    #                 facet_grid(region ~ sex) +
    #                 scale_colour_manual(values = getPalette(colourCount), name="Incomde Quintiles") +
    #                 baseplot
  # 
  #     # save Plot
  #     ggsave(graf3, file="graf3 Regional LifeEx.png" )
  #   
  
  
  
  ## test conditional colors -----------------------
  ## test conditional colors -----------------------
  ## test conditional colors -----------------------
  ## test conditional colors -----------------------
  
fig_4b  
  ggplot(LIFETABLE_reg_quint, aes(x = factor(x), y = log(nqx), group=tableid)) +
    geom_line(data=LIFETABLE_reg_quint, aes( group=tableid), color = "gray", alpha= .5) +
    geom_line(data=subset(LIFETABLE_reg_quint, tableid %like% 1 | tableid %like% 5), aes( group=tableid), col="red") +
    facet_grid(region ~ sex) +
    baseplot +
    theme(legend.position="top") +
    scale_x_discrete(name="Age") +
    # scale_color_brewer( palette="Oranges", name = "Income Quintile")
    scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) # Gay scale
  
  
  
  
  ggplot(LIFETABLE_reg_quint, aes(x = factor(x), y = log(nqx), group=tableid)) +
    geom_line(data=LIFETABLE_reg_quint, aes( group=tableid), color = "gray", alpha= .6) +
    geom_line(data=subset(LIFETABLE_reg_quint, tableid %like% 1 | tableid %like% 5), aes( group=tableid, col=tableid %like% 5)) +
    facet_grid(region ~ sex) +
    baseplot +
    theme(legend.position="top") +
    scale_x_discrete(name="Age") +
    scale_colour_manual(name = 'Income Quinties', values = setNames(c('blue','red'), c(T,F)))
  
  
head( root_table_reg )
  
ggplot(root_table_reg, aes(x = age85, y = log(nMx), group=tableid)) +
  geom_line(data=root_table_reg, aes( group=tableid), color = "gray", alpha= .6) +
  geom_line(data=subset(root_table_reg, tableid %like% 1 | tableid %like% 5), 
            aes( group=tableid, col=tableid %like% 5)) +
  facet_grid(region ~ sex) +
  baseplot +
  theme(legend.position="top") +
  scale_x_discrete(name="Age") +
  scale_colour_manual(name = 'Income Quinties', values = setNames(c('blue','red'), c(T,F)))

  
  


  
  ggplot(LIFETABLE_reg_quint, aes(x = factor(x), y = log(nqx), group=tableid)) +
    geom_line(data=LIFETABLE_reg_quint, aes( group=tableid), color = "gray", alpha= .5) +
    geom_line(data=subset(LIFETABLE_reg_quint, tableid %like% 1 | tableid %like% 5), 
              aes( group=tableid, col = ifelse(tableid %like% 1,'red', ifelse(tableid %like% 5,'dddddddd','grey')))) +
    facet_grid(region ~ sex) +
    baseplot +
    theme(legend.position="top") +
    scale_x_discrete(name="Age")  
  