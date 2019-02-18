# laoding 
library(tidyverse)
library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(scales)
library(devtools)
library(plotly)
library(data.table)
library(scales)
library(extrafont)




# graph 3: race/ethnicity 

# prepare data 
# 2019 data
demo_2019 <- 
  read_excel("data/demo_special/Demographics_LEPSPED_2019.xls", 
             sheet = "Networks", 
             range = cell_rows(4:25),
             col_names = c("Network", "Population", "Bi_no", "Bi_per", "SpEd_no", 
                           "SpEd_per", "FreeLunch_no", "FreeLunch_per"))
demo_2019$year <- 
  rep(2019,nrow(demo_2019))

# 2018 data
demo_2018 <- 
  read_excel("data/demo_special/Demographics_LEPSPED_2018.xls", 
             sheet = "Networks", 
             range = cell_rows(4:21),
             col_names = c("Network", "Population", "Bi_no", "Bi_per", "SpEd_no", 
                           "SpEd_per", "FreeLunch_no", "FreeLunch_per"))
demo_2018$year <- 
  rep(2018,nrow(demo_2018))

# 2017 data
demo_2017 <- read_excel("data/demo_special/Demographics_LEPSPED_2017.xls", 
                        sheet = "Networks", 
                        range = cell_rows(4:22),
                        col_names = c("Network", "Population", "Bi_no", "Bi_per", "SpEd_no", 
                                      "SpEd_per", "FreeLunch_no", "FreeLunch_per"))
demo_2017$year <- 
  rep(2017,nrow(demo_2017))

# combine dataset from all years
demo_all <- 
  bind_rows(demo_2019, demo_2018, demo_2017) 
demo_all <-
  demo_all[demo_all$Bi_per >= 0.15,]

# rename cell 
demo_all$Network <- 
  gsub("Service Leadership Academies", "SLA", demo_all$Network)

# convert values to numeric and percentage
demo_all$Bi_per <- 
  as.numeric(as.character(demo_all$Bi_per)) * 100
demo_all$SpEd_per <- 
  as.numeric(as.character(demo_all$SpEd_per)) * 100
demo_all$FreeLunch_per <- 
  as.numeric(as.character(demo_all$FreeLunch_per)) * 100


# draw graph
lunch_bi <- 
  ggplot(demo_all, aes(x = FreeLunch_per, 
                       y = Bi_per)) +
  
  geom_point(alpha = 1, 
             aes(color = Network), 
             size = 3) +
  
  geom_smooth(method = 'lm',
              formula = y~x, 
              se = FALSE) +
  
  geom_hline(data = subset(demo_all, year == 2019), 
             aes(yintercept = mean(Bi_per), 
                 group = year), 
             linetype = "dashed", 
             color = "#f25f5c", 
             size = .5) +
  
  geom_hline(data = subset(demo_all, year == 2018), 
             aes(yintercept = mean(Bi_per), 
                 group = year), 
             linetype ="dashed", 
             color = "#f25f5c", 
             size=.5) +
  
  geom_hline(data = subset(demo_all, year == 2017), 
             aes(yintercept = mean(Bi_per), 
                 group = year), 
             linetype ="dashed", 
             color = "#f25f5c", 
             size=.5) +
  
  geom_vline(data = subset(demo_all, year == 2019), 
             aes(xintercept = mean(FreeLunch_per), 
                 group = year), 
             linetype = "dashed", 
             color = "#5ed7bf", 
             size =.5) +
  
  geom_vline(data = subset(demo_all, year == 2018), 
             aes(xintercept = mean(FreeLunch_per), 
                 group = year), 
             linetype = "dashed", 
             color = "#5ed7bf", 
             size =.5) +
  
  geom_vline(data = subset(demo_all, year == 2017), 
             aes(xintercept = mean(FreeLunch_per), 
                 group = year), 
             linetype = "dashed", 
             color = "#5ed7bf", 
             size =.5) +
  
  facet_wrap( ~ year, 
              nrow =1, 
              labeller = as_labeller(c("2017" = "FY 1617", 
                                       "2018" = "FY 1718", 
                                       "2019" = "FY 1819"))) +
  
  scale_color_manual(values = c("Charter" = "#F59AA3", 
                                "Network 1" = "#ffa45c", 
                                "Network 2" =  "#34a7b2",
                                "Network 3" = "#5b2e35",
                                "Network 4" = "#a7d7c5",
                                "Network 6" = "#ffe0e0",
                                "Network 7" = "#caabd8",
                                "Network 8" = "#fffa67",
                                "Network 10" = "#a2eae2",
                                "ISP" = "#b5525c")) +
  
  xlab("% Free/Reduced Lunch") + 
  ylab("% Bilingual") +
  
  xlim(50, 95) + 
  ylim(5, 50) +
  
  annotate("label",  
           x = 50, 
           y = 48,  
           label = "Green: avg for Free Lunch \nRed: avg for Bilingual", 
           size = 3, 
           hjust = 0) +
  
  labs(
    title = "Networks with More Bilingual Population are also Networks \nwith more Economically Disadvantaged Population",
    subtitle = "Distributions of 2017-2019, only for Networks' with more than 15% bilingual population\n", 
    caption = "CPS School Data Report") +
  
  theme(
    plot.title = element_text(size = 18, 
                              hjust = 0.5, 
                              face = "bold", 
                              family = "Concert One"),
    plot.subtitle = element_text(size = 16, 
                                 hjust = 0.5, 
                                 family = "Crimson Text"),
    plot.caption = element_text(size = 12, 
                                hjust = 1, 
                                family = "Lobster"), 
    
    axis.title.x = element_text(size = 14, 
                                face = "bold", 
                                family = "Crimson Text" ), 
    axis.title.y = element_text(size = 14, 
                                face = "bold", 
                                family = "Crimson Text" ),
    
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    
    panel.grid.major.y = element_line(size = 0.2, 
                                      linetype = 'solid',
                                      colour = "lightgray"), 
    panel.grid.major.x = element_line(size = 0.2,  
                                      linetype = 'solid',
                                      colour = "lightgray"), 
    panel.background = element_blank(),
    
    
    legend.position = "bottom",
    legend.spacing.x = unit(0.5, 'cm'),
    legend.text = element_text(size = 10, 
                               face = "bold", 
                               family = "Crimson Text"),
    legend.title = element_blank(), 
    
    strip.background = element_blank(), 
    strip.text.x = element_text(size = 15, 
                                face = "bold", 
                                color = "#3c4f65"))

lunch_bi
