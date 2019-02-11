# laoding 
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(reshape2)


# read in file 
progress_2019 <- read_csv("Chicago_Public_Schools_-_School_Progress_Reports_SY1819.csv", col_names = TRUE)
progress_2019 <- select(progress_2019, School_ID, Short_Name, starts_with('NWEA'))
progress_2019 <- select(progress_2019, School_ID, Short_Name, ends_with('Pct'))
progress_2019 <- select(progress_2019, School_ID, Short_Name, contains('Growth'))
progress_2019 <- progress_2019[complete.cases(progress_2019), ]
colnames(progress_2019) <- c("ID", "Name", "Reading_3", "Reading_4", "Reading_5", "Reading_6", "Reading_7", "Reading_8", 
                             "Math_3", "Math_4", "Math_5", "Math_6", "Math_7", "Math_8")

progress_2019 <- melt(progress_2019, id=c("ID","Name"))
progress_2019$subject <- ifelse(grepl("Math", progress_2019$variable), "Math", "Reading")
progress_2019$variable <- gsub('Math_3', '3', progress_2019$variable)
progress_2019$variable <- gsub('Reading_3', '3', progress_2019$variable)
progress_2019$variable <- gsub('Math_4', '4', progress_2019$variable)
progress_2019$variable <- gsub('Reading_4', '4', progress_2019$variable)
progress_2019$variable <- gsub('Math_5', '5', progress_2019$variable)
progress_2019$variable <- gsub('Reading_5', '5', progress_2019$variable)
progress_2019$variable <- gsub('Math_6', '6', progress_2019$variable)
progress_2019$variable <- gsub('Reading_6', '6', progress_2019$variable)
progress_2019$variable <- gsub('Math_7', '7', progress_2019$variable)
progress_2019$variable <- gsub('Reading_7', '7', progress_2019$variable)
progress_2019$variable <- gsub('Math_8', '8', progress_2019$variable)
progress_2019$variable <- gsub('Reading_8', '8', progress_2019$variable)


# draw graph

progress <- ggplot(progress_2019, aes(x= variable, y = value)) +
  geom_violin(trim = FALSE)+
  geom_jitter(position=position_jitter(0.1),  
              alpha = 0.5, 
              aes(color = subject == "Reading")) +
  facet_wrap( ~ subject,nrow = 1) +
  stat_summary(fun.y=median, geom="line", aes(group=1))  + 
  stat_summary(fun.y=median, geom="point") +
  scale_color_manual(labels = c("Math", "Reading"), 
                     values = c("TRUE" = "#FBF4B1", "FALSE" = "#FFCBCB")) +
  xlab("Grades") + ylab("NWEA Growth (50 Stays Same)") + 
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "CPS Students are Making Progress in both Math and Reading \n especially for Grade 7 and 8", 
    subtitle = "SY1819, NWEA Growth for Math and Reading for Students in Grade 3 - 8", 
    caption = "CPS School Data Report: School Progress Reports SY1819.",
    color = "Subject") +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold", family = "Helvetica"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, family = "Palatino"),
    plot.caption = element_text(size = 8, hjust = 1, face="bold.italic"), 
    axis.title.x = element_text(size=12, face="bold"), 
    axis.title.y = element_text(size=12, face="bold"),
    strip.text.x = element_text(size = 10, face="bold"),
    panel.background = element_blank())
  
  

progress