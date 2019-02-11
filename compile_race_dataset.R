race <- read_excel("Racial_Ethnic_Survey.xlsx", sheet = "Sheet1")
race$Other <- race$`Asian/Pacific Islander` + race$`Native American` + race$`Multi-Racial` +
  race$Asian + race$`Hawaiian/Pacific Islander` + race$`Not Available`
var_list <- c("Year", "Other", "Hispanic", "African American", "White")
race <- race[var_list]
race <- melt(race, id.var="Year")
colnames(race) <- c("Year", "Ethnicity", "Percentage")
race$Percentage <- lapply(race$Percentage, round, 1)