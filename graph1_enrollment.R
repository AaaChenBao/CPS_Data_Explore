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
library(here)



# function - generate new variables 
gen_var <- function(df, year){
  df$year <- year 
  df$kindergarten <- df["PE"] + df["PK"] + df["K"]
  df$elementary <- df["01"] + df["02"] + df["03"] + df["04"] + 
    df["05"] + df["06"] + df["07"] + df["08"]
  df$high <- df["09"] + df["10"] + df["11"] + df["12"]  
  var_list <- c('year', 'kindergarten', 'elementary', 'high')
  df <- df[var_list]
  df <- sapply( df, as.numeric )
  return(df)
}



# read in files
enroll_2019 <- 
  read_excel(here("data/enrollment", 
                  "Demographics_20thDay_2019.xls"), 
             sheet = "Schools")
enroll_2019 <- 
  enroll_2019[enroll_2019$"School Name" == "District Total 2018-2019",]
enroll_2019 <- gen_var(enroll_2019, 2019)


enroll_2018 <-
  read_excel("data/enrollment/Demographics_20thDay_2018.xls", sheet = "Schools")
enroll_2018 <-
  enroll_2018[enroll_2018$"School Name" == "District Total 2017-2018",]
enroll_2018 <- gen_var(enroll_2018, 2018)


enroll_2017 <- 
  read_excel("data/enrollment/Demographics_20thDay_2017.xls", sheet = "Schools")
enroll_2017 <-
  enroll_2017[enroll_2017$"School Name" == "District Total 2016-2017",]
enroll_2017 <- gen_var(enroll_2017, 2017)


enroll_2016 <- 
  read_excel("data/enrollment/Demographics_20thDay_2016.xls", sheet = "Sheet1")
enroll_2016 <-
  enroll_2016[enroll_2016$"Network" == "District Totals",]
enroll_2016 <-
  enroll_2016[rowSums( is.na(enroll_2016) ) <= 10, ]
enroll_2016 <- gen_var(enroll_2016, 2016)


enroll_2015 <- 
  read_excel("data/enrollment/Demographics_20thDay_2015.xls", sheet = "Sheet1")
enroll_2015 <-
  enroll_2015[enroll_2015$"Network" == "District Totals",]
enroll_2015 <-
  enroll_2015[rowSums( is.na(enroll_2015) ) <= 10, ]
enroll_2015 <- gen_var(enroll_2015, 2015)


enroll_2014 <- 
  read_excel("data/enrollment/Demographics_20thDay_2014.xls", sheet = "enrollment_20th_day_2014")
enroll_2014 <-
  enroll_2014[enroll_2014$"Network" == "District Totals",]
enroll_2014 <-
  enroll_2014[rowSums( is.na(enroll_2014) ) <= 10, ]
enroll_2014 <- gen_var(enroll_2014, 2014)


enroll_2013 <- 
  read_excel("data/enrollment/Demographics_20thDay_2013.xls", sheet = "enrollment_20th_day_2013")
enroll_2013 <-
  enroll_2013[enroll_2013$"Network" == "District Total",]
enroll_2013 <-
  enroll_2013[rowSums( is.na(enroll_2013) ) <= 10, ]
enroll_2013 <- gen_var(enroll_2013, 2013)


enroll_2012 <- 
  read_excel("data/enrollment/Demographics_20thDay_2012.xls", sheet = "enrollment_20th_day_2012")
enroll_2012 <-
  enroll_2012[enroll_2012$"Network" == "District Totals",]
enroll_2012 <-
  enroll_2012[rowSums( is.na(enroll_2012) ) <= 10, ]
enroll_2012 <- gen_var(enroll_2012, 2012)


enroll_2011 <- 
  read_excel("data/enrollment/Demographics_20thDay_2011.xls", sheet = "enrollment_20th_day")
enroll_2011 <-
  enroll_2011[enroll_2011$"Area" == "District Totals",]
enroll_2011 <-
  enroll_2011[rowSums( is.na(enroll_2011) ) <= 10, ]
enroll_2011 <- gen_var(enroll_2011, 2011)


enroll_2010 <- 
  read_excel("data/enrollment/Demographics_20thDay_2010.xls", sheet = "Sheet1")
enroll_2010 <-
  enroll_2010[enroll_2010$"Area" == "District Totals",]
enroll_2010 <-
  enroll_2010[rowSums( is.na(enroll_2010) ) <= 10, ]
enroll_2010 <- gen_var(enroll_2010, 2010)


enroll_2009 <- 
  read_excel("data/enrollment/Demographics_20thDay_2009.xls", sheet = "Query1")
enroll_2009 <- 
  enroll_2009[enroll_2009$"Area" == "Dsitrict Totals",]
enroll_2009 <-
  enroll_2009[rowSums( is.na(enroll_2009) ) <= 10, ]
enroll_2009 <- gen_var(enroll_2009, 2009)


enroll_2008 <- 
  read_excel("data/enrollment/Demographics_20thDay_2008.xls", sheet = "Sheet1")
enroll_2008 <- 
  enroll_2008[enroll_2008$"Area" == "District Totals",]
enroll_2008 <-
  enroll_2008[rowSums( is.na(enroll_2008) ) <= 10, ]
enroll_2008$K <- enroll_2008["Full-Day\nK"] + enroll_2008["Half-Day\nK"]
enroll_2008$"02" <- enroll_2008["02'"]
enroll_2008 <- gen_var(enroll_2008, 2008)

enroll_2007 <- 
  read_excel("data/enrollment/Demographics_20thDay_2007.xls", sheet = "Sheet1")
enroll_2007 <- 
  enroll_2007[enroll_2007$"Area" == "District Totals",]
enroll_2007 <-
  enroll_2007[rowSums( is.na(enroll_2007) ) <= 10, ]
enroll_2007$PE <- enroll_2007["Head\nStart"]
enroll_2007$PK <- enroll_2007["Other\nPK"] + enroll_2007["State\nPK"] + enroll_2007["PK\nSPED"]
enroll_2007$K <- enroll_2007["Full-Day\nK"] + enroll_2007["Half-Day\nK"]
enroll_2007 <- gen_var(enroll_2007, 2007)


enroll_2006 <- 
  read_excel("data/enrollment/Demographics_20thDay_2006.xls", sheet = "enrollment_0608")
enroll_2006 <- 
  enroll_2006[enroll_2006$"Area" == "District Totals",]
enroll_2006 <-
  enroll_2006[rowSums( is.na(enroll_2006) ) <= 10, ]
enroll_2006$PE <- enroll_2006["Head\nStart"]
enroll_2006$PK <- enroll_2006["Other\nPK"] + enroll_2006["State\nPK"] + enroll_2006["PK\nSPED"]
enroll_2006$K <- enroll_2006["Full-Day\nK"] + enroll_2006["Half-Day\nK"]
enroll_2006 <- gen_var(enroll_2006, 2006)



enroll_all = bind_rows(enroll_2019, enroll_2018, enroll_2017, enroll_2016, enroll_2015,
                    enroll_2014, enroll_2013, enroll_2012, enroll_2011, enroll_2010,
                    enroll_2009, enroll_2008, enroll_2007, enroll_2006) 
enroll_all$'total population' <- enroll_all$kindergarten + enroll_all$elementary + enroll_all$high


enroll_all <- enroll_all[c("year", "total population", "kindergarten", "elementary", "high")]
colnames(enroll_all) <- c("year", "Total Population", "Kindergarten", "Elementary School", "High School")


enroll_all <- melt(enroll_all, id.var="year")
colnames(enroll_all) <- c("Year", "Student_Type", "Headcount")


# draw graph
enrollment <- ggplot(enroll_all, aes(x= Year, y = Headcount)) +
  geom_point(aes(color=Student_Type)) +
  geom_line(aes(color=Student_Type)) +
  geom_text(data=subset(enroll_all,Year == 2006), aes(label = Headcount), size = 3, vjust = 2, hjust = 0.3) +  
  geom_text(data=subset(enroll_all,Year == 2019), aes(label = Headcount), size = 3, vjust = -0.9, hjust = 0.5) +  
  facet_wrap( ~ Student_Type, scales = "free_y", nrow = 4) +
  scale_x_continuous(breaks=seq(2006, 2019, 1)) +
  scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3)) +
  xlab("Year") + ylab("Enrollment Headcount") + 
  labs(
    title = "Chicago Public Schools Enrollment Drops by 50,000 Students in the Past 10 Years ", 
    subtitle = "Enrollment drops for all types of students, from kindergarten to high school", 
    caption = "CPS School Data Report 2010-2019: 20th Day Membership.") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold", family = "Helvetica"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, family = "Palatino"),
    plot.caption = element_text(size = 8, hjust = 1, face="bold.italic"), 
    axis.title.x = element_text(size=12, face="bold"), 
    axis.title.y = element_text(size=12, face="bold"),
    strip.text.x = element_text(size = 10, face="bold"),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',
                                    colour = "grey"), 
    legend.position = "bottom",
    legend.title = element_blank())

enrollment 

