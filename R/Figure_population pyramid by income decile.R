

          # # load data people 2010
          # load("./data/censo2010_BRpes.Rdata")
          # setDT(censo2010_BRpes)
          # 
          # 
          # # Generate Population count tables, by Region, State, Sex, age85 and Income group  (approx. 2 minutes)
          #   pop_table2010 <- censo2010_BRpes[ , .(pop2=sum(V0010)), by=.(V1001,V0601,decileBR,quintileBR,quintileRegion, age85)]
          #   pop_table2010 <- subset(pop_table2010, pop2 >0 )
          # 
          # # Change name of variables
          #   colnames(pop_table2010) <- c("region",  "sex", "decileBR", "quintileBR", "quintileRegion", "age85", "pop2")
          #   table(pop_table2010$age85)
          # 
          # df <- copy(pop_table2010)

df <- copy(root_table_nat)
setDT(df)


# calculate relative pop by age/sex
  df[, pop_prop := population / sum(population), by=.(decileBR)]
  df[, death_prop := death_count / sum(death_count), by=.(decileBR)]
  
  # 66666666666666666666666
  df[, taxa := death_count / population ]
  df$taxa <- ifelse(df$sex == "Men", -1*df$taxa, df$taxa)
  summary(df$taxa)
  
  # Population size in each income decile
  df[, .(sum(population)), by=decileBR][order(decileBR)]
  df[, .(sum(pop_prop)), by=decileBR][order(decileBR)]
  

# barplots for male populations goes to the left (thus negative sign)
  df$population <- ifelse(df$sex == "Men", -1*df$population, df$population)
  df$pop_prop <- ifelse(df$sex == "Men", -1*df$pop_prop, df$pop_prop)
  
  df$death_count <- ifelse(df$sex == "Men", -1*df$death_count, df$death_count)
  df$death_prop <- ifelse(df$sex == "Men", -1*df$death_prop, df$death_prop)
  
  
# lables for income deciles
df[, decileBR := factor(decileBR, levels=c(1:10), labels = c("D1 - poorest 10%","D2","D3","D4",'D5','D6','D7','D8','D9', "D10 - richest 10%"))]
  

  

  
  
# Figure. Population pyramid by income decile

# abs numbers
ggplot(df, aes(x = age85, y = population, fill = sex)) + 
    geom_bar(data=subset(df, sex == "Men"), stat = "identity") + 
    geom_bar(data=subset(df, sex == "Women"), stat = "identity") + 
    scale_y_continuous(breaks = seq(-4000000, 4000000, 1000000),
                       labels = paste0(as.character(c(4:0, 1:4)), "m")) +
    coord_flip() + 
    scale_fill_brewer(palette = "Set1", direction = -1) + 
    theme_bw() +
    facet_wrap(~decileBR, ncol = 5)
  

# Proportion
temp_plot <- 
  ggplot(df, aes(x = age85, y = pop_prop, fill = sex)) + 
    geom_bar(data=subset(df, sex == "Men"), stat = "identity") + 
    geom_bar(data=subset(df, sex == "Women"), stat = "identity") + 
    # scale_y_continuous(breaks = seq(-0.06, 0.06, .02),
    #                    labels = paste0(as.character(c(4:0, 1:4)), "m")) +
    scale_y_continuous(breaks = seq(-0.06, 0.06, .02),
                       labels= scales::percent) +
    coord_flip() + 
    scale_fill_brewer(palette = "Set1", direction = -1) + 
    theme_bw() +
    facet_wrap(~decileBR, ncol = 5)
  
  

ggsave(temp_plot, file="./plots/pop_pyramid.png", dpi = 800,
       width = 30, height = 15, units = "cm")




# deaths
temp_plot <- 
  ggplot(df, aes(x = age85, y = death_prop, fill = sex)) + 
  geom_bar(data=subset(df, sex == "Men"), stat = "identity") + 
  geom_bar(data=subset(df, sex == "Women"), stat = "identity") + 
  # scale_y_continuous(breaks = seq(-0.06, 0.06, .02),
  #                    labels = paste0(as.character(c(4:0, 1:4)), "m")) +
  scale_y_continuous(#breaks = seq(-0.06, 0.06, .02),
                     labels= scales::percent) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1", direction = -1) + 
  theme_bw() +
  facet_wrap(~decileBR, ncol = 5)



ggsave(temp_plot, file="./plots/deaths_pyramid.png", dpi = 800,
       width = 30, height = 15, units = "cm")


# TAXA
temp_plot <- 
  ggplot(df, aes(x = age85, y = taxa, fill = sex)) + 
  geom_bar(data=subset(df, sex == "Men"), stat = "identity") + 
  geom_bar(data=subset(df, sex == "Women"), stat = "identity") + 
  # scale_y_continuous(breaks = seq(-0.06, 0.06, .02),
  #                    labels = paste0(as.character(c(4:0, 1:4)), "m")) +
  scale_y_continuous(#breaks = seq(-0.06, 0.06, .02),
    labels= scales::percent) +
  coord_flip() + 
  scale_fill_brewer(palette = "Set1", direction = -1) + 
  theme_bw() +
  facet_wrap(~decileBR, ncol = 5)


# population structure



  
  
  
  
  