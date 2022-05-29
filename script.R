COVID19_line_list_data <- read.csv("/cloud/project/COVID19_line_list_data.csv")
library(Hmisc)
describe(COVID19_line_list_data)
#cleaned data since some data in date form
COVID19_line_list_data$death_dummy <- as.integer(COVID19_line_list_data$death != 0)

#to find death rate
sum(COVID19_line_list_data$death_dummy)/nrow(COVID19_line_list_data)

#now we have to prove person who dies is older person


alive <- subset(COVID19_line_list_data, death_dummy == 0)
dead <- subset(COVID19_line_list_data, death_dummy == 1)
mean(alive$age, na.rm = TRUE)
mean(dead$age, na.rm = TRUE)
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)
#since we can see our p-value is almost 0. if p value less than 0.5 we have to accept null hypothesis if p is greater than 0.5 
#we have to accept alternative hypothesis
#therefore as conclusion people die is older guy


#lets make another hypothesis
#we have to test is male death is high compared to female

men <- subset(COVID19_line_list_data, gender == "male")

women <- subset(COVID19_line_list_data, gender == "female")


mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)

t.test(men$death_dummy, female$death_dummy, alternative = "two.sided", conf.level = 0.99)
#since p value less than 0.5 therefore null hypothesis is true