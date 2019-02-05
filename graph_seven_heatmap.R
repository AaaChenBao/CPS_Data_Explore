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

progress_2019 <- 
  read_csv("progress_report/Chicago_Public_Schools_-_School_Progress_Reports_SY1819.csv", 
           col_names = TRUE) %>%
  mutate(`School_ID` = as.character(`School_ID`))


SQRP_2019 <- read_excel("Accountability_SQRPratings_2018-2019_SchoolLevel.xls", sheet = "Elem Schools (grds PreK-8 only)", 
                   skip = 1)%>%
  mutate(`School ID` = as.character(`School ID`))

demo_2019 <- 
  read_excel("demo_special/Demographics_LEPSPED_2019.xls", 
             sheet = "Schools",
             skip = 1) %>%
  mutate(`School ID` = as.character(`School ID`))

all_2019 <-
  progress_2019 %>%
    inner_join(demo_2019, by = c("School_ID" = "School ID")) %>%
    inner_join(SQRP_2019, by = c("School_ID" = "School ID")) %>%
    filter((Primary_Category == "ES")) %>%
    select("Zip",  "SQRP Total Points Earned",
           starts_with("Attainment"),
           -starts_with("Attainment_All_Grades"),
           -ends_with('Lbl_ES'),
           -ends_with('School_Lbl'),
           -contains('SAT'),
           starts_with("School_Survey_Student"),
           starts_with("School_Survey_Teacher"),
           starts_with("Student_Attendance"),
           starts_with("Teacher_Attendance"),
           -ends_with('2_Pct'),
           -contains('Avg'),
           "%..6", "%..8", "%..10") 

all_2019 <- all_2019[complete.cases(all_2019), ] 

all_2019 <- 
  mutate_all(all_2019, function(x) as.numeric(as.character(x)))

names(all_2019) <- c("Zip", "SQRP", "Reading", "Math", "Student Response", "Teacher Responce", 
                     "Student Attendance",  
                     "Teacher Attendance", 
                     "Bilingual", "Special Ed", "Free Lunch")

correlation <- 
  round(cor(all_2019),2)


get_lower_tri<-function(correlation){
  correlation[upper.tri(correlation)] <- NA
  return(correlation)
}


get_upper_tri <- function(correlation){
  correlation[lower.tri(correlation)]<- NA
  return(correlation)
}

lower_tri <- get_lower_tri(correlation)
half_correlation <- melt(lower_tri, na.rm = TRUE)

higher_tri <- get_upper_tri(correlation)
other_half_correlation <- melt(higher_tri, na.rm = TRUE)


correlation_map <- 
  ggplot(data = half_correlation, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white", alpha = 0.9) +
  geom_text(data = other_half_correlation, aes(Var2, Var1, label = value), color = "white", size = 3) +
  geom_text(data = subset(half_correlation, value == 1), aes(label = Var1),  vjust= -3.1, hjust= 0.7, size = 3) + 
  scale_fill_gradient2(low = "#FFF0BC", high = "#35234B", mid = "#D84C73", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation Matrix") +
  coord_cartesian(clip = 'off') +
  labs(
    title = "Strong Positive Association: SQRP with Math & Reading Attainment",
    subtitle = "Correlation Heatmap, Also Significant Negetive Association: % Free Lunch & Math & Reading Attainment", 
    caption = "CPS School Data Report: Limited English Proficiency, Special Ed, Low Income, IEP. \n CPS School Data Report: School Quality Rating Policy Results and Accountability Status. \n CPS School Data Report: School Progress Reports SY1819.") +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.5,
                               title.position = "top", title.hjust = 0.5)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, vjust = 1.2, size = 8, hjust = 1),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold", family = "Helvetica"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, family = "Palatino"),
    plot.caption = element_text(size = 8, hjust = 1, face="bold.italic"), 
    strip.text.x = element_text(size = 10, face="bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.3, 0.7),
    legend.direction = "horizontal")
    
  
correlation_map