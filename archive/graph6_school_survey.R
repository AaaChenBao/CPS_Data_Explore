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

filter_column <- function(df){
  df <- select(df, 
               contains('School_Survey'), 
               -ends_with('Pct'),
               -ends_with('Description'))
  return(df)
}



generate_count <- function(df, year){
  
  Involved_Families <- count(df, "School_Survey_Involved_Families")
  Involved_Families$type <- 'Involved Families'
  colnames(Involved_Families) <- c("degree", "count", "type")
  
  Supportive_Environment <- count(df, "School_Survey_Supportive_Environment")
  Supportive_Environment$type <- 'Supportive Environment'
  colnames(Supportive_Environment) <- c("degree", "count", "type")
  
  Ambitious_Instruction <- count(df, "School_Survey_Ambitious_Instruction")
  Ambitious_Instruction$type <- 'Ambitious Instruction'
  colnames(Ambitious_Instruction) <- c("degree", "count", "type")
  
  Effective_Leaders <- count(df, "School_Survey_Effective_Leaders")
  Effective_Leaders$type <- 'Effective Leaders'
  colnames(Effective_Leaders) <- c("degree", "count", "type")
  
  Collaborative_Teachers <- count(df, "School_Survey_Collaborative_Teachers")
  Collaborative_Teachers$type <- 'Collaborative Teachers'
  colnames(Collaborative_Teachers) <- c("degree", "count", "type")
  
  Safety <- count(df, "School_Survey_Safety")
  Safety$type <- 'Safety'
  colnames(Safety) <- c("degree", "count", "type")
  
  School_Community <- count(df, "School_Survey_School_Community")
  School_Community$type <- 'School Community'
  colnames(School_Community) <- c("degree", "count", "type")
  
  Parent_Teacher_Partnership <- count(df, "School_Survey_Parent_Teacher_Partnership")
  Parent_Teacher_Partnership$type <- 'Parent Teacher Partnership'
  colnames(Parent_Teacher_Partnership) <- c("degree", "count", "type")
  
  Quality_Of_Facilities <- count(df, "School_Survey_Quality_Of_Facilities")
  Quality_Of_Facilities$type <- 'Quality Of Facilities'
  colnames(Quality_Of_Facilities) <- c("degree", "count", "type")
  
  survey_one_year <- bind_rows(Involved_Families, Supportive_Environment, Ambitious_Instruction, Effective_Leaders, 
                               Collaborative_Teachers, Safety, School_Community, Parent_Teacher_Partnership, Quality_Of_Facilities)
  
  survey_one_year$year <- year
  
  return(survey_one_year)
}



progress_2019 <- read_csv("progress_report/Chicago_Public_Schools_-_School_Progress_Reports_SY1819.csv", col_names = TRUE)
progress_2019 <- filter_column(progress_2019)
progress_2019 <- generate_count(progress_2019, 2019)

progress_2018 <- read_csv("progress_report/Chicago_Public_Schools_-_School_Progress_Reports_SY1718.csv", col_names = TRUE)
progress_2018 <- filter_column(progress_2018)
progress_2018 <- generate_count(progress_2018, 2018)

progress_2017 <- read_csv("progress_report/Chicago_Public_Schools_-_School_Progress_Reports_SY1617.csv", col_names = TRUE)
progress_2017 <- filter_column(progress_2017)
progress_2017 <- generate_count(progress_2017, 2017)

progress_2016 <- read_csv("progress_report/Chicago_Public_Schools_-_School_Progress_Reports_SY1516.csv", col_names = TRUE)
progress_2016 <- filter_column(progress_2016)
progress_2016 <- generate_count(progress_2016, 2016)

survey <- bind_rows(progress_2019, progress_2018, progress_2017, progress_2016)
survey <- survey[c("type", "year", "degree", "count")]
colnames(survey) <- c("group", "year", "degree", "value")
survey <- survey[complete.cases(survey), ]
survey$value <- as.numeric(survey$value)
survey$group <- as.factor(survey$group)
survey$degree <- as.factor(survey$degree)
survey$degree <- revalue(survey$degree, c("Neutral"="NEUTRAL"))
survey$degree <- revalue(survey$degree, c("Strong"="STRONG"))
survey$degree <- revalue(survey$degree, c("Very strong"="VERY STRONG"))
survey$degree <- revalue(survey$degree, c("Very weak"="VERY WEAK"))
survey$degree <- revalue(survey$degree, c("Weak"="WEAK"))

survey$group <- revalue(survey$group, c("Involved Families"="A"))
survey$group <- revalue(survey$group, c("Supportive Environment"="B"))
survey$group <- revalue(survey$group, c("Ambitious Instruction"="C"))
survey$group <- revalue(survey$group, c("Effective Leaders"="D"))
survey$group <- revalue(survey$group, c("Collaborative Teachers"="E"))
survey$group <- revalue(survey$group, c("Safety"="F"))
survey$group <- revalue(survey$group, c("School Community"="G"))
survey$group <- revalue(survey$group, c("Parent Teacher Partnership"="H"))
survey$group <- revalue(survey$group, c("Quality Of Facilities"="I"))
survey$group <- as.factor(survey$group)

survey_2019 <- survey[(survey$year == '2019'),]
survey_2019$id <- seq.int(nrow(survey_2019))

# Set a number of 'empty bar' to add at the end of each group
empty_bar=2
to_add = data.frame(matrix(NA, empty_bar*nlevels(survey_2019$group), ncol(survey_2019)) )
colnames(to_add) = colnames(survey_2019)
to_add$group=rep(levels(survey_2019$group), each=empty_bar)
survey_2019=rbind(survey_2019, to_add)
survey_2019=survey_2019 %>% arrange(group)
survey_2019$id=seq(1, nrow(survey_2019))


# Get the name and the y position of each label
label_data=survey_2019
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# label_data_angle
label_data_angle <-
  label_data %>%
  group_by(group) %>%
  summarise(group_angle = median(angle))


# prepare a data frame for base lines
base_data=survey_2019 %>% 
  group_by(group) %>%
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))


# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1


# Make the plot
survey_plot <- ggplot(survey_2019, aes(x=as.factor(id), y=value)) +     
  geom_bar(aes(x=as.factor(id), y=value, fill=degree), stat="identity", alpha=0.8, width = 0.95) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 300, xend = start, yend = 300), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  annotate("text", x = rep(max(survey_2019$id),4), y = c(100, 200, 300, 400), 
           label = c("100", "200", "300", "400") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  scale_fill_manual(values=c("#D3BDA2","#615B59","#DB9A96","#DBB2AF", "#E5CAC5", "#E7DFE0")) +
  ylim(-200,350) +
  labs(
    title = "Schools are not Promoting Safety and School Community", 
    subtitle = "School are having Effective Leaders, Collaborative Teachers and Ambitious Instruction", 
    caption = "CPS School Data Report: School Progress Reports SY1819.") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold", family = "Helvetica"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, family = "Palatino"),
    plot.caption = element_text(size = 8, hjust = 1, face="bold.italic"), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right") +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=value, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=2.5, 
            angle= label_data$angle, inherit.aes = FALSE ) +
  geom_text(data=base_data, aes(x = title, y = -18, label = group), 
            colour = "black", alpha= 0.8, size= 2, fontface="bold", 
            inherit.aes = FALSE)


survey_plot

