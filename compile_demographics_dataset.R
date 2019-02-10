# laoding 
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(reshape2)



# 2019 data
demo_2019 <- read_excel("demographics/Demographics_LEPSPED_2019.xls", sheet = "Networks", range = cell_rows(4:25),
                        col_names = c("Network", "Population", "Bi_no", "Bi_per", "SpEd_no", 
                                      "SpEd_per", "FreeLunch_no", "FreeLunch_per"))
demo_2019$year <- rep(2019,nrow(demo_2019))

# 2018 data
demo_2018 <- read_excel("demographics/Demographics_LEPSPED_2018.xls", sheet = "Networks", range = cell_rows(4:21),
                        col_names = c("Network", "Population", "Bi_no", "Bi_per", "SpEd_no", 
                                      "SpEd_per", "FreeLunch_no", "FreeLunch_per"))
demo_2018$year <- rep(2018,nrow(demo_2018))

# 2017 data
demo_2017 <- read_excel("demographics/Demographics_LEPSPED_2017.xls", sheet = "Networks", range = cell_rows(4:22),
                        col_names = c("Network", "Population", "Bi_no", "Bi_per", "SpEd_no", 
                                      "SpEd_per", "FreeLunch_no", "FreeLunch_per"))
demo_2017$year <- rep(2017,nrow(demo_2017))


# combine dataset from all years
demo_all = bind_rows(demo_2019, demo_2018, demo_2017) 
demo_all = demo_all[demo_all$Bi_per >= 0.15,]

# rename cell 
demo_all$Network <- gsub("Service Leadership Academies", "SLA", demo_all$Network)

# convert values to numeric and percentage
demo_all$Bi_per <- as.numeric(as.character(demo_all$Bi_per)) * 100
demo_all$SpEd_per <- as.numeric(as.character(demo_all$SpEd_per)) * 100
demo_all$FreeLunch_per <- as.numeric(as.character(demo_all$FreeLunch_per)) * 100


write.csv(demo_all, file = "demo_all.csv", row.names = FALSE)
