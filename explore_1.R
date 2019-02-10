# laoding 
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(reshape2)

# Bar chart - Use the title to tell me why this matters! I can look at the axis titles to see what the chart is. This is especially important since... I don't know why this chart matters. 
# Do you expect your viewers to know which networks are which? Are they clustered geographically in some what that lets you tell me something more informative than just their number?
# The dotted red line is also entirely unlabeled. While I took a minute to surmise it was the average across networks, most people won't. Include this as a direct label, rather than part of the subtitle. 
# Title and subtitle text is a bit small, relative to the graph size.
# "Variable" is not an appropriate name for the categorical color scale for school type. 
# You can remove extra y-axis space (between bars and labels) with: + scale_y_continuous(expand = c(0, 0))


# Bubble chart - Switch the title and subtitle please.
# I'd add '%' signs to the x ans y axis tick mark labels.
# I'm worried about occlusion. Did you consider adding alpha (for transparency) so you can see circles hidden in the clump?
# You can also improve this by giving the graph more space, again with:
# + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
# Also, you could move the legends to within the top left corner of the graph, making more room for it to expand down.
# Lastly: you are using a continuous color scale for your school quality rating. Looking at the legend, it's not clear how many values there are (2,3,4? or 1,2,3,4,5?). Instead of this, use an ordinal color scale. Five different colors (that appear ordered, like the color wheel) for the numbers 1-2-3-4-5 (assuming I'm guessing right that there are 5 ratings).
# You can do this with:  https://ggplot2.tidyverse.org/reference/scale_manual.html


# Small multiple scatterplots: 
# Too many colors! I cannot tell the difference between several of the greens, the pinks, the blues, or the oranges.  Instead of this, label just a few of the more interesting dots and tell the viewer why this all matters.
# Mentioning averages is fine, but the subtitle is not the place to do that. In a scatterplot you could do this with a dotted line, or with a dot just for averages. That said, it's not clear to me why you're including this information. 
# I'm not sure I think your subtitle is true here. There are clearly many high FRL networks with low billingual percentages.

# graph 1 
# read in files
demo_all <- read_csv("demo_all.csv", col_names = TRUE)


# draw graph
lunch_bi <- ggplot(demo_all,  aes(x = FreeLunch_per, y = Bi_per)) +
  geom_point(alpha = 1, aes(color=Network)) +
  geom_smooth(method='lm',formula=y~x) +
  geom_hline(data=subset(demo_all, year == 2019), 
             aes(yintercept = mean(Bi_per), group = year), linetype="dashed", color = "#f25f5c", size=.5) +
  geom_hline(data=subset(demo_all, year == 2018), 
             aes(yintercept = mean(Bi_per), group = year), linetype="dashed", color = "#f25f5c", size=.5) +
  geom_hline(data=subset(demo_all, year == 2017), 
             aes(yintercept = mean(Bi_per), group = year), linetype="dashed", color = "#f25f5c", size=.5) +
  geom_vline(data=subset(demo_all, year == 2019), 
             aes(xintercept = mean(FreeLunch_per), group = year), linetype="dashed", color = "#5ed7bf", size=.5) +
  geom_vline(data=subset(demo_all, year == 2018), 
             aes(xintercept = mean(FreeLunch_per), group = year), linetype="dashed", color = "#5ed7bf", size=.5) +
  geom_vline(data=subset(demo_all, year == 2017), 
             aes(xintercept = mean(FreeLunch_per), group = year), linetype="dashed", color = "#5ed7bf", size=.5) +
  facet_wrap( ~ year, nrow =1) +
  xlab("% Free/Reduced Lunch") + ylab("% Bilingual") +
  xlim(50, 95) + ylim(5, 50) +
  labs(
    title = "Networks with More Bilingual Pooulation are also Networks \n with more Economically Disadvantaged Population",
    subtitle = "Distributions of 2017-2019, only for Networks' with more than 15% bilingual population", 
    caption = "CPS School Data Report: Limited English Proficiency, Special Ed, Low Income, IEP.") +
  theme(
    aspect.ratio=1, 
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold", family = "Helvetica"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, family = "Palatino"),
    plot.caption = element_text(size = 8, hjust = 1, face="bold.italic"), 
    axis.title.x = element_text(size=12, face="bold"), 
    axis.title.y = element_text(size=12, face="bold"),
    strip.text.x = element_text(size = 10, face="bold"),
    panel.background = element_blank())

lunch_bi

ggsave("lunch_bi.pdf")






# revision for Grpah 2, enrollment by over time 

# read in files
race <- read_excel("Racial_Ethnic_Survey.xlsx", sheet = "Sheet1")
race$Other <- race$`Asian/Pacific Islander` + race$`Native American` + race$`Multi-Racial` +
  race$Asian + race$`Hawaiian/Pacific Islander` + race$`Not Available`
var_list <- c("Year", "Other", "Hispanic", "African American", "White")
race <- race[var_list]
race <- melt(race, id.var="Year")
colnames(race) <- c("Year", "Ethnicity", "Percentage")
race$Percentage <- lapply(race$Percentage, round, 1)

# draw graph
race_bar <- ggplot(race, aes(x= Year, y = Percentage, fill = Ethnicity, label = Percentage)) +
  coord_flip() +
  geom_bar(stat = "identity", alpha = 0.9) +
  geom_text(data=race, size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#6C5B7B", "#F8B195", "#F67280", "#C06C84")) + 
  scale_x_continuous(breaks=seq(2010, 2019, 1), expand = c(.01, 0)) +
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
    panel.background = element_blank())

  
race_bar




  geom_text(data=subset(enroll_network,value > 20), size = 3, position = position_stack(vjust = 0.5)) +
  xlab("Networks") + ylab("Enrollment") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=c("355C7D","F8B195", "F67280", "C06C84")) +
  labs(title = "CPS FY1819 20th Day Enrollment Breakdown by Networks", 
       subtitle = "Average Network Enrollmment is 12766.
       Networks specialize in providing Elementary or High Shcool Education", 
       caption = "CPS School Data Report: Racial/Ethnic Report.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

enrollment


# graph 2 
# read in files
Network_Info_1 <- read_excel("Accountability_SQRPratings_2018-2019_SchoolLevel.xls", sheet = "Elem Schools (grds PreK-8 only)", 
                             range = cell_cols(1:3),
                             col_names = c("School ID", "Name", "Network"))
Network_Info_1 <- Network_Info_1[-(1:3), , drop = FALSE]

Network_Info_2 <- read_excel("Accountability_SQRPratings_2018-2019_SchoolLevel.xls", sheet = "High Schools (grds 9-12 only)", 
                             range = cell_cols(1:3),
                             col_names = c("School ID", "Name", "Network"))
Network_Info_2 <- Network_Info_2[-(1:3), , drop = FALSE]

Network_Info_3 <- read_excel("Accountability_SQRPratings_2018-2019_SchoolLevel.xls", sheet = "Combo Schools (grds PreK-12)", 
                             range = cell_cols(1:3),
                             col_names = c("School ID", "Name", "Network"))
Network_Info_3 <- Network_Info_3[-(1:3), , drop = FALSE]

Network_Info_4 <- read_excel("Accountability_SQRPratings_2018-2019_SchoolLevel.xls", sheet = "Option Schools", 
                             range = cell_cols(1:3),
                             col_names = c("School ID", "Name", "Network"))
Network_Info_4 <- Network_Info_4[-(1:3), , drop = FALSE]

enroll <- read_excel("enrollment/Demographics_20thDay_2019.xls", sheet = "Schools")
enroll <- enroll[complete.cases(enroll), ]

# combine dataset from all school types
Network_Info = bind_rows(Network_Info_1, Network_Info_2, Network_Info_3, Network_Info_4) 

enroll <- merge(enroll, Network_Info, by = "School ID" )

enroll <- enroll[ , -which(names(enroll) %in% c("School ID", "School Name", "Total", "Name"))]

enroll_network <- aggregate( . ~ Network, enroll, sum)

enroll_network <- enroll_network[enroll_network$Network!='Charter' & enroll_network$Network!='AUSL' 
                                 & enroll_network$Network!='Contract' & enroll_network$Network!='ISP'
                                 & enroll_network$Network!='Options', ]

enroll_network["Kindergarten"] <- enroll_network["PE"] + enroll_network["PK"] + enroll_network["K"]
enroll_network["Elementary"] <- enroll_network["01"] + enroll_network["02"] + enroll_network["03"] + enroll_network["04"] + 
  enroll_network["05"] + enroll_network["06"] + enroll_network["07"] + enroll_network["08"]
enroll_network["High"] <- enroll_network["09"] + enroll_network["10"] + enroll_network["11"] + enroll_network["12"]  

enroll_network <- enroll_network[order(enroll_network$Kindergarten),]

enroll_network <- enroll_network[ , which(names(enroll_network) %in% c("Network", "High", "Elementary", "Kindergarten"))]
enroll_network <- melt(enroll_network, id.var="Network")


# draw graph
enrollment <- ggplot(enroll_network, aes(x=reorder(Network, value), y = value, fill = variable, label = value)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_hline(yintercept=12765.88, linetype="dashed", color = "red", size=1) + 
  geom_text(data=subset(enroll_network,value > 20), size = 3, position = position_stack(vjust = 0.5)) +
  xlab("Networks") + ylab("Enrollment") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=c("lightpink","lightskyblue", "lightgoldenrod1")) +
  labs(title = "CPS FY1819 20th Day Enrollment Breakdown by Networks", 
       subtitle = "Average Network Enrollmment is 12766.
       Networks specialize in providing Elementary or High Shcool Education", 
       caption = "CPS School Data Report: 20th Day Membership.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

enrollment

ggsave("enrollment.pdf")



# graph 3
# read in files
# 2019 SQRP
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
    title = "High School SQRP Ratings are Heavily Determined by \n Graduation, Attendance, and College Enrollment", 
    subtitle = "CPS FY1819 High School SQRP Ratings vs. Graduation, Attendance, and College Enrollment", 
    caption = "CPS School Data Report: School Quality Rating Policy Results and Accountability Status
       *Outlier removed for High School with missing values and extreme values") +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold", family = "Helvetica"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, family = "Palatino"),
    plot.caption = element_text(size = 8, hjust = 1, face="bold.italic"), 
    axis.title.x = element_text(size=12, face="bold"), 
    axis.title.y = element_text(size=12, face="bold"),
    strip.text.x = element_text(size = 10, face="bold"),
    panel.background = element_blank()) + 
  theme(legend.position = "bottom", legend.direction = "horizontal") 
   

sqrp_grad_attend


ggsave("sqrp_grad_attend.pdf")

