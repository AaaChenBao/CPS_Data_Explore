# Static graph style 

## caption content 
caption = "CPS School Data Report"
caption = "City of Chicago Data Portal"


## subtitle content 
## add "\n" in the end when really close

## title, subtitle, caption style 


plot.title = element_text(size = 18, hjust = 0.5, face = "bold", family = "Concert One"),
plot.subtitle = element_text(size = 14, hjust = 0.5, family = "Bitter"),
plot.caption = element_text(size = 12, hjust = 1, family = "Lobster"), 


## axis title
axis.title.x = element_text(size=14, face="bold", family =  "Crimson Text" ), 
axis.title.y = element_text(size=14, face="bold", family =  "Crimson Text" ),

axis.title.x = element_blank(),
axis.title.y = element_blank(),


## axis text
axis.text.x = element_text(angle = 25, vjust = 1.2, size = 8, hjust = 1),
axis.text.y = element_blank(),


## legend position
legend.position = "bottom",
legend.spacing.x = unit(0.5, 'cm'),
legend.text = element_text(size=10, face="bold"),