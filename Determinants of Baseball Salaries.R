# Libraries
library(readr)
library(dplyr)

# Import Data
Salaries <- read_csv("Salaries (1).csv")
Pitching <- read_csv("Pitching (1).csv")
Fielding <- read_csv("Fielding (1).csv") 
Batting <- read_csv("Batting (1).csv")

# Sample Period: 1985 - 2016
Batting <- Batting %>% 
  filter(yearID >= 1985 & yearID <= 2016)

# Sample period: Year 2016
Batting_2016 <- Batting %>% filter(yearID == 2016)
Salaries_2016 <- Salaries %>% filter(yearID == 2016)
Pitching_2016 <- Pitching %>% filter(yearID == 2016)

# For Hitters
Batting_Salaries_2016 <- merge(Batting_2016,
                               Salaries_2016,
                               by.x = "playerID",
                               by.y = "playerID")

# For Pitchers
Pitching_Salaries_2016 <- merge(Pitching_2016,
                                Salaries_2016,
                                by.x = "playerID",
                                by.y = "playerID")

write.csv(Batting_Salaries_2016, "Batting_Salaries_2016.csv")
write.csv(Pitching_Salaries_2016, "Pitching_Salaries_2016.csv")





######### Regression

library(readr)
mydata <- read_csv("Batting_Salaries_2016.csv")

drop = c("X1", "yearID.y", "teamID.y", "lgID.y", "stint")
mydata = mydata[ ,!(names(mydata) %in% drop)]

# Hits (H, 2B, 3B, HR) / AB
library(dplyr)
mydata <- mydata %>% 
  mutate(BA = (H+`2B`+`3B`+HR)/AB)

######################
# Stepwise regression
#1. salary ~ H
#2. salary ~ H + `2B`
#3. salary ~ H + `2B` + `3B`
#4. salary ~ H + `2B` + `3B` + HR

lm(mydata$salary ~ mydata$H)
lm(mydata$salary ~ mydata$H + mydata$`2B`)
lm(mydata$salary ~ mydata$H + mydata$`2B` + mydata$`3B`)
lm(mydata$salary ~ mydata$H + mydata$`2B` + mydata$`3B` + mydata$HR)

summary(lm(salary ~ H + `2B` +`3B` + HR, data = mydata))

#Correlation
cor(mydata$salary, mydata$H)
cor(mydata$salary, mydata$`2B`)
cor(mydata$salary, mydata$`3B`)
cor(mydata$salary, mydata$HR)

#Covariance
cov(mydata$salary, mydata$H)
cov(mydata$salary, mydata$`2B`)
cov(mydata$salary, mydata$`3B`)
cov(mydata$salary, mydata$HR)

my_vars = mydata %>% select(salary, H, `2B`, `3B`, HR)
cor = cor(my_vars)
mycolors = colorRampPalette(c("cyan", "blue"))
heatmap(cor, col = mycolors(100))

########################