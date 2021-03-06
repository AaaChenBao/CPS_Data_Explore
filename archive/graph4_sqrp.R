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

# graph 4
# read in files
SQRP_2019 <- 
  read_excel(here("data/accountability_SQRP", 
                  "Accountability_SQRPratings_2018-2019_SchoolLevel.xls"), 
             sheet = "High Schools (grds 9-12 only)", 
             skip = 1) 

SQRP_2019 <- 
  select(SQRP_2019, "School ID", "School Name", 
                    "SQRP Total Points Earned", 
                    "4-Year Cohort Graduation Rate",  
                    "Average Daily Attendance Rate", 
                    "College Enrollment Rate") %>% 
  drop_na() %>% 
  mutate_at(vars(ends_with("Rate")),
            funs(as.numeric)) %>% 
  filter("4-Year Cohort Graduation Rate" != 0 & 
         "Average Daily Attendance Rate" != 0 |
         "College Enrollment Rate" != 0) 


names(SQRP_2019) <- 
  c("ID", "Name", "SQRP_Score", 
    "Graduation", "College_enroll",  "Attendance")

SQRP <- SQRP_2019[SQRP_2019$Graduation!=0 & SQRP_2019$Attendance!=0 & SQRP_2019$College_enroll!=0, ]


# draw graph
sqrp_grad_attend <- ggplot(SQRP, aes(x = Graduation, y = Attendance, size = College_enroll, fill = SQRP_Score)) +
  geom_point(shape = 21) + 
  xlab("% 4-Year Cohort Graduation Rate") + ylab("% Average Daily Attendance Rate") +
  labs(size = "% College Enrollment Rate", fill = "School Quality Rating") +
  scale_x_continuous(limits=c(20, 100), breaks=c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits=c(70, 100), breaks=c(70, 75, 80, 85, 90, 95, 100)) +
  scale_size(range = c(0,6),
             breaks = c(30, 40, 50, 60, 70, 80, 90, 100),
             labels = c(30, 40, 50, 60, 70, 80, 90, 100)) +
  labs(
    title = "High School SQRP Ratings are Heavily Determined by \nGraduation, Attendance, and College Enrollment", 
    subtitle = "CPS FY1819 High School SQRP Ratings vs. Graduation, Attendance, and College Enrollment\n", 
    caption = "CPS School Data Report \n*Outlier removed for High School with missing values and extreme values") +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold", family = "Concert One"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, family = "Bitter"),
    plot.caption = element_text(size = 12, hjust = 1, family = "Lobster"), 
    axis.title.x = element_text(size=12, face="bold"), 
    axis.title.y = element_text(size=12, face="bold"),
    strip.text.x = element_text(size = 10, face="bold"),
    panel.background = element_blank()) + 
  theme(legend.position = "bottom", legend.direction = "horizontal") 

sqrp_grad_attend