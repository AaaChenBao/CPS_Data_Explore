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


column_name_1 <- c('type', 'total',
                   'white', 'w_per', 'african american', 'a_per', 'pacific', 'p_per', 
                   'native american', 'n_per', 'hispanic', 'h_per', 'multi', 'm_per', 
                   'asian', 'as_per', 'hawaiian', 'ha_per', 'na', 'na_per')

column_name_2 <- c('type', 'total',
                   'white', 'w_per', 'african american', 'a_per', 'native american', 'n_per',
                   'pacific', 'p_per', 'hispanic', 'h_per')


column_name_3 <- c('type', 'total',
                   'white', 'w_per', 'african american', 'a_per', 'native american', 'n_per',
                   'pacific', 'p_per', 'hispanic', 'h_per', 'multi', 'm_per')



# function - generate new variables 
gen_var <- function(df, year, column){
  df <- df[rowSums(is.na(df)) < 10, ]
  colnames(df) <- column 
  df$type <- NULL
  df$Year <- year 
  df$African_American <- as.numeric(df["african american"]) / as.numeric(df["total"]) * 100
  df$Hispanic <- as.numeric(df["hispanic"]) / as.numeric(df["total"]) * 100
  df$White <- as.numeric(df["white"]) / as.numeric(df["total"]) * 100
  if (("asian" %in% names(df)) && ("multi" %in% names(df)))
  {
    df$Asian <- as.numeric(df["asian"]) / as.numeric(df["total"]) * 100;
    df$Other <- (as.numeric(df["pacific"]) + as.numeric(df["native american"]) + as.numeric(df["multi"]) + 
                   as.numeric(df["hawaiian"]) + as.numeric(df["na"])) / as.numeric(df["total"]) * 100;
  }
  else if ((!"asian" %in% names(df)) && (!"multi" %in% names(df)))
  {
    df$Asian <- 0;
    df$Other <- (as.numeric(df["pacific"]) + as.numeric(df["native american"])) / as.numeric(df["total"]) * 100;
  }
  else if  ((!"asian" %in% names(df)) && ("multi" %in% names(df)))
  {
    df$Asian <- 0;
    df$Other <- (as.numeric(df["pacific"]) + as.numeric(df["native american"]) + as.numeric(df["multi"])) / as.numeric(df["total"]) * 100
  }
  var_list <- c('African_American', 'Hispanic', 'White', 'Asian', 'Other', 'Year')
  df <- df[var_list]
  return(df)
}






# read in files
race_2019 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2019.xls", sheet = "Grades", skip = 1) 
race_2019 <- 
  race_2019[race_2019$"Grade Level" == "District Total",]
race_2019 <- gen_var(race_2019, 2019, column_name_1)


race_2018 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2018.xls", sheet = "Grades", skip = 1) 
race_2018 <- 
  race_2018[race_2018$"Grade Level" == "District Total",]
race_2018 <- gen_var(race_2018, 2018, column_name_1)


race_2017 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2017.xls", sheet = "Grades", skip =1) 
race_2017 <- 
  race_2017[race_2017$"Grade Level" == "District Total",]
race_2017 <- gen_var(race_2017, 2017, column_name_1)


race_2016 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2016.xls", sheet = "Grades", skip =1) 
race_2016 <- 
  race_2016[race_2016$"Grade Level" == "District Totals",]
race_2016 <- gen_var(race_2016, 2016, column_name_1)


race_2015 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2015.xls", sheet = "Grades", skip =1) 
race_2015 <- 
  race_2015[race_2015$"Grade Level" == "District Totals",]
race_2015 <- gen_var(race_2015, 2015, column_name_1)


race_2014 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2014.xls", sheet = "Grades", skip =1) 
race_2014 <- 
  race_2014[race_2014$"Grade Level" == "District Totals",]
race_2014 <- gen_var(race_2014, 2014, column_name_1)

race_2013 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2013.xls", sheet = "Grades", skip =1) 
race_2013 <- 
  race_2013[race_2013$"Grade Level" == "District Totals",]
race_2013 <- gen_var(race_2013, 2013, column_name_1)

race_2012 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2012.xls", sheet = "Grades", skip =1) 
race_2012 <- 
  race_2012[race_2012$"Grade Level" == "District Totals",]
race_2012 <- gen_var(race_2012, 2012, column_name_1)


race_2011 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2011.xls", sheet = "Grades", skip =1) 
race_2011 <- 
  race_2011[race_2011$"..1" == "District Totals",]
race_2011 <- gen_var(race_2011, 2011, column_name_1)


race_2010 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2010.xls", sheet = "Grades", skip =1) 
race_2010 <- 
  race_2010[race_2010$"..1" == "Dsitrict Totals",]
race_2010 <- gen_var(race_2010, 2010, column_name_2)



race_2009 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2009.xls", sheet = "Grades", skip =1) 
race_2009 <- 
  race_2009[race_2009$"..1" == "District Totals",]
race_2009 <- gen_var(race_2009, 2009, column_name_2)



race_2008 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2008.xls", sheet = "Grades", skip =1, range = cell_cols("A:N")) 
race_2008 <- 
  race_2008[race_2008$"..1" == "Grand Total",]
race_2008 <- gen_var(race_2008, 2008, column_name_3)


race_2007 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2007.xls", sheet = "Totals_by_Grades", skip =1, range = cell_cols("A:N")) 
race_2007 <- 
  race_2007[race_2007$"..1" == "Grand Total",]
race_2007 <- gen_var(race_2007, 2007, column_name_3)


race_2006 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2006.xls", sheet = "Totals by Grade", skip =1, range = cell_cols("A:N")) 
race_2006 <- 
  race_2006[race_2006$"..1" == "GRAND TOTAL",]
race_2006 <- gen_var(race_2006, 2006, column_name_3)


race_2005 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2005.xlsx", sheet = "School Types", skip =1, range = cell_cols("B:M")) 
race_2005 <- 
  race_2005[race_2005$"..1" == "Grand Total",]
race_2005 <- gen_var(race_2005, 2005, column_name_2)



race_2004 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2004.xls", sheet = "Totals by Types", skip =1, range = cell_cols("B:M")) 
race_2004 <- 
  race_2004[race_2004$"..1" == "Grand Total",]
race_2004 <- gen_var(race_2004, 2004, column_name_2)


race_2003 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2003.xls", sheet = "Totals by Type", skip =1, range = cell_cols("B:M")) 
race_2003 <- 
  race_2003[race_2003$"..1" == "Grand Total",]
race_2003 <- gen_var(race_2003, 2003, column_name_2)


race_2002 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2002.xls", sheet = "Totals by Types", skip =1, range = cell_cols("B:M")) 
race_2002 <- 
  race_2002[race_2002$"..1" == "Grand Total",]
race_2002 <- gen_var(race_2002, 2002, column_name_2)


race_2001 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2001.xls", sheet = "Totals by Type", skip =1, range = cell_cols("B:M")) 
race_2001 <- 
  race_2001[race_2001$"..1" == "Grand Total",]
race_2001 <- gen_var(race_2001, 2001, column_name_2)


race_2000 <- 
  read_excel("demo_racial/Demographics_RacialEthnic_2000.xls", sheet = "Totals by Type", skip =1, range = cell_cols("B:M")) 
race_2000 <- 
  race_2000[race_2000$"..1" == "Totals",]
race_2000 <- gen_var(race_2000, 2000, column_name_2)



race = bind_rows(race_2019, race_2018, race_2017, race_2016, race_2015,
                       race_2014, race_2013, race_2012, race_2011, race_2010, race_2009,
                       race_2008, race_2007, race_2006, race_2005, race_2004, race_2003,
                       race_2002, race_2001, race_2000) 

race$African_American <- -(race$African_American)
race <- race[c('African_American','White',  'Hispanic', 'Year')]

race <- melt(race, id.var="Year")
colnames(race) <- c("Year", "Ethnicity", "Percentage")
race$Percentage <- round(race$Percentage, digits = 2) 


# draw graph
race_bar <- ggplot(race, aes(x= Year, y = Percentage, group = Ethnicity, 
                             fill = factor(Ethnicity, levels = c('African_American','Hispanic',  'White')), 
                             label = sprintf("%0.2f", round(Percentage, digits = 2)))) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.95) + 
  geom_text(data=subset(race, Ethnicity == 'African_American'), aes(label = sprintf("%0.2f", round(abs(Percentage), digits = 2))), 
            size = 3.5, position = position_stack(vjust = 0.3)) +
  geom_text(data=subset(race, Ethnicity != 'African_American'), size = 3.5, position = position_stack(vjust = 0.7)) +

  coord_flip() +
  scale_x_discrete(limits = rev(race$Year), expand = c(0, 0)) +
  scale_fill_manual(values=c("#f16821","#feffdf","#fab95b")) +
  scale_y_continuous(breaks = (seq(-60, 60, 10)), 
                     labels = abs(seq(-60, 60, 10))) +
  labs(
    title = "More than 80% Chicago Public Schools Students are \n African American and Hispanic Students", 
    subtitle = "From 2010-2019, relatively more white students and students with other race enrolled in CPS", 
    caption = "CPS School Data Report 2010-2019: Racial/Ethnic Report.") +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold", family = "Helvetica"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, family = "Palatino"),
    plot.caption = element_text(size = 8, hjust = 1, face="bold.italic"), 
    axis.title.x = element_text(size=12, face="bold"), 
    axis.title.y = element_text(size=12, face="bold"),
    strip.text.x = element_text(size = 10, face="bold"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank())

race_bar



# race_bar <- ggplot(race, aes(x= Year, y = Percentage, fill = Ethnicity, label = Percentage)) +
#   coord_flip() +
#   geom_bar(stat = "identity", alpha = 0.9) +
#   geom_text(data=race, size = 3, position = position_stack(vjust = 0.5)) +
#   scale_fill_manual(values=c("#6C5B7B", "#F8B195", "#F67280", "#C06C84", 'red')) + 
#   scale_x_continuous(breaks=seq(2000, 2019, 1), expand = c(.01, 0)) +
#   labs(
#     title = "More than 80% Chicago Public Schools Students are \n African American and Hispanic Students", 
#     subtitle = "From 2010-2019, relatively more white students and students with other race enrolled in CPS", 
#     caption = "CPS School Data Report 2010-2019: Racial/Ethnic Report.") +
#   theme(
#     plot.title = element_text(size = 13, hjust = 0.5, face = "bold", family = "Helvetica"),
#     plot.subtitle = element_text(size = 11, hjust = 0.5, family = "Palatino"),
#     plot.caption = element_text(size = 8, hjust = 1, face="bold.italic"), 
#     axis.title.x = element_text(size=12, face="bold"), 
#     axis.title.y = element_text(size=12, face="bold"),
#     strip.text.x = element_text(size = 10, face="bold"),
#     axis.text.x = element_text(angle = 0, hjust = 1),
#     panel.background = element_blank())
# 
# race_bar

