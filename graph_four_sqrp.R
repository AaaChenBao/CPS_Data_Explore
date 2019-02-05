





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


# graph 4
# read in files
SQRP <- read_excel("Accountability_SQRPratings_2018-2019_SchoolLevel.xls", sheet = "High Schools (grds 9-12 only)", 
                   skip = 1)
SQRP <- SQRP[ , which(names(SQRP) %in% c("School ID", "School Name", "SQRP Total Points Earned", 
                                         "4-Year Cohort Graduation Rate",  "Average Daily Attendance Rate", "College Enrollment Rate"))]
SQRP <- SQRP[complete.cases(SQRP), ]  
names(SQRP) <- c("ID", "Name", "SQRP_Score", "Graduation", "College_enroll",  "Attendance")

SQRP$Graduation <- as.numeric(as.character(SQRP$Graduation))
SQRP$Attendance <- as.numeric(as.character(SQRP$Attendance)) 
SQRP$College_enroll <- as.numeric(as.character(SQRP$College_enroll)) 

SQRP <- SQRP[SQRP$Graduation!=0 & SQRP$Attendance!=0 & SQRP$College_enroll!=0, ]


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