

############ set working Directory ------------------
setwd("R:/Dropbox/life expectancy (inequality)/income/ineq_LifeExp shared") # set working Directory



##################### Load package85s -------------------------------------------------------

source("./Rscripts/_LoadPackages.R")


# load data
root_table <- fread('./results/root_table.csv')

 
 root_table_nat <- root_table[, lapply(.SD, sum, na.rm=TRUE), by=c("sex", "quintileBR", "age85"), .SDcols=c("population", "death_count") ] 
 
 root_table_nat[ , nMx := death_count / population ]
 root_table_nat[, age85 := factor(age85, ordered = TRUE,
                              levels = c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                                         "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                                         "70-74","75-79","80-84","85+"))]
 
 
 root_table_nat <- subset(root_table_nat, quintileBR %in% c(1,3,5))
 
# Population
 ggplot(root_table_nat) + 
   geom_line(aes(x=age85, y=population, color=factor(quintileBR), group=quintileBR)) +
   scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) +# Gay scale
   facet_grid( sex~. )  # vertical
   
 
 
 
 # Deaths
 ggplot(root_table_nat) + 
   geom_line(aes(x=age85, y=death_count , color=factor(quintileBR), group=quintileBR)) +
 #  scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) +# Gay scale
   facet_grid( sex~. )  # vertical
 
 
 # nMx
 ggplot(subset(root_table_nat)) + 
   geom_line(aes(x=age85, y=log(nMx) , color=factor(quintileBR), group=quintileBR)) +
#  scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) + # Gay scale
   facet_grid( sex~. )  # vertical
 
 ggplotly()
 
 library(plotly)
 
 
################################################## LIFE tables
 
 LIFETABLE_nat_quint <- fread('./results/LIFETABLE_nat_quint.csv')
 LIFETABLE_nat_quint[, longevity := x +ex ]
 
 
 
 LIFETABLE_nat_quint <- subset(LIFETABLE_nat_quint, quintileBR %in% c(1,3,5))
 
 
 # Longevity
 ggplot(data= LIFETABLE_nat_quint, aes(y=longevity, x=factor(x))) +
   geom_point(aes(color=factor(quintileBR), label=tableid), size=2 ) +
   facet_grid(.~sex ) + # vertical
   # baseplot +
   theme(legend.position="top") +
   scale_y_continuous(name="Longevity", limits=c(60, 100)) # +
   # scale_x_discrete(name="Region") +
   #scale_color_brewer( palette="Oranges", name = "Income Quintile")
  # scale_colour_grey( start = 0.8, end = 0, guide = guide_legend(title = "Income Quintile")) # Gay scale
 
 
 # qx
 ggplot(data= LIFETABLE_nat_quint, aes(y=log(nqx), x=factor(x))) +
   geom_point(aes(color=factor(quintileBR), label=tableid), size=2 ) +
   facet_grid( sex~. )  # vertical
 
 ggplotly()
 
 
 
 
 
 
 
 
 
 
 
 
 