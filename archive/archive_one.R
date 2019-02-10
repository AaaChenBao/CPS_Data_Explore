# laoding 
library('tidyverse')
library('dplyr')
library('readxl')
library('ggplot2')
library('gridExtra')
library("reshape2")

install.packages('tidyverse')
# graph 1 
# read in files
# 2019 data
demo_2019 <- read_excel("Demographics_LEPSPED_2019.xls", sheet = "Networks", range = cell_rows(4:25),
                        col_names = c("Network", "Population", "Bi_no", "Bi_per", "SpEd_no", 
                                      "SpEd_per", "FreeLunch_no", "FreeLunch_per"))
demo_2019$year <- rep(2019,nrow(demo_2019))

# 2018 data
demo_2018 <- read_excel("Demographics_LEPSPED_2018.xls", sheet = "Networks", range = cell_rows(4:21),
                        col_names = c("Network", "Population", "Bi_no", "Bi_per", "SpEd_no", 
                                      "SpEd_per", "FreeLunch_no", "FreeLunch_per"))
demo_2018$year <- rep(2018,nrow(demo_2018))

# 2017 data
demo_2017 <- read_excel("Demographics_LEPSPED_2017.xls", sheet = "Networks", range = cell_rows(4:22),
                        col_names = c("Network", "Population", "Bi_no", "Bi_per", "SpEd_no", 
                                      "SpEd_per", "FreeLunch_no", "FreeLunch_per"))
demo_2017$year <- rep(2017,nrow(demo_2017))


# combine dataset from all years
demo_all = bind_rows(demo_2019, demo_2018, demo_2017) 


# rename cell 
demo_all$Network <- gsub("Service Leadership Academies", "SLA", demo_all$Network)

# convert values to numeric and percentage
demo_all$Bi_per <- as.numeric(as.character(demo_all$Bi_per)) * 100
demo_all$SpEd_per <- as.numeric(as.character(demo_all$SpEd_per)) * 100
demo_all$FreeLunch_per <- as.numeric(as.character(demo_all$FreeLunch_per)) * 100


# draw graph
lunch_bi <- ggplot(demo_all, aes(x = FreeLunch_per, y = Bi_per, col = Network)) +
  geom_jitter(alpha = 0.9) +
  facet_wrap( ~ year, nrow =1) +
  xlab("% Free/Reduced Lunch") + ylab("% Bilingual") +
  xlim(50, 100) + ylim(0, 50) +
  labs(title = "CPS Networks Percentage Free/Reduced Lunch vs.Percentage Bilingual 2017 - 2019", 
       subtitle = "Average % free/reduced lunch is 80.03. Average % Blingual is 15.68. 
       Networks with higher % free/reduced lunch population are assoicated with higher % bilingual population", 
       caption = "CPS School Data Report: Limited English Proficiency, Special Ed, Low Income, IEP.") +
  theme(aspect.ratio=1)

lunch_bi

ggsave("lunch_bi.pdf")



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

enroll <- read_excel("Demographics_20thDay_2019.xls", sheet = "Schools")
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
  xlab("4-Year Cohort Graduation Rate") + ylab("Average Daily Attendance Rate") +
  labs(size = "College Enrollment Rate", fill = "School Quality Rating") +
  scale_x_continuous(limits=c(20, 100), breaks=c(20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_y_continuous(limits=c(70, 100), breaks=c(70, 75, 80, 85, 90, 95, 100)) +
  scale_size(range = c(0, 12),
             breaks = c(30, 40, 50, 60, 70, 80, 90, 100),
             labels = c(30, 40, 50, 60, 70, 80, 90, 100)) +
  scale_fill_continuous(low = "khaki1", high = "red2") +
  labs(title = "CPS FY1819 High School SQRP Ratings vs. Graduation, Attendance, and College Enrollment", 
       subtitle = "High school SQRP ratings are heavily determined by graduation, attendance, and college enrollment.", 
       caption = "CPS School Data Report: School Quality Rating Policy Results and Accountability Status
       *Outlier removed for High School with missing values and extreme values") +
  theme(legend.position = "bottom", legend.direction = "horizontal") 


sqrp_grad_attend


ggsave("sqrp_grad_attend.pdf")