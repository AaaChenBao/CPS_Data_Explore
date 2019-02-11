# laoding 
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(scales)
library(ggmap)
library(maps)
library(mapdata)
library(devtools)



register_google(key = 'AIzaSyCoubxGnaNap2nOMyOy94ZZo1tmatsI40s')


chicago <- get_map("Chicago", source='google')

progress_2019 <- read_csv("Chicago_Public_Schools_-_School_Progress_Reports_SY1819.csv", col_names = TRUE) 




chicago <- get_map("Chicago", 
                   maptype = "toner",
                   scale = 4,
                   source = "stamen")




cook_county <- ggplot(data = cook, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.7) + 
  geom_polygon(color = "black", fill = "gray") + 
  geom_point(progress_2019, aes(x= School_Longitude, y = School_Latitude))

cook_county


points(airports$lon, airports$lat, col = "red", cex = .6)


salesCalls <- data.frame(State=rep("louisiana",5), 
                         City=c("Baton Rouge", "New Orleans", "Shreveport", 
                                "Lafayette", "Mandeville"),
                         Calls=c(10,5,8,13,2))















# graph 1 
# read in file 
enroll_all <- read_csv("enroll_all.csv", col_names = TRUE)



# draw graph
enrollment <- ggplot(enroll_all, aes(x= Year, y = Headcount)) +
  geom_point(aes(color=Student_Type)) +
  geom_line(aes(color=Student_Type)) +
  geom_text(data=subset(enroll_all,Year == 2010), aes(label = Headcount), size = 3, vjust = 2, hjust = 0.3) +  
  geom_text(data=subset(enroll_all,Year == 2019), aes(label = Headcount), size = 3, vjust = -0.9, hjust = 0.5) +  
  facet_wrap( ~ Student_Type, scales = "free_y", nrow = 4) +
  scale_x_continuous(breaks=seq(2010, 2019, 1)) +
  scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3)) +
  xlab("Year") + ylab("Enrollment Headcount") + 
  labs(
    title = "Chicago Public Schools Enrollment Drops by 50,000 Students in the Past 10 Years ", 
    subtitle = "Enrollment drops for all types of students, from kindergarten to high school", 
    caption = "CPS School Data Report 2010-2019: 20th Day Membership.") +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold", family = "Helvetica"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, family = "Palatino"),
    plot.caption = element_text(size = 8, hjust = 1, face="bold.italic"), 
    axis.title.x = element_text(size=12, face="bold"), 
    axis.title.y = element_text(size=12, face="bold"),
    strip.text.x = element_text(size = 10, face="bold"),
    panel.background = element_blank())
  
enrollment  
