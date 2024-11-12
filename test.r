# useful library we are doing to use a lot
library(tidyverse)

# some summaries of the overall data
glimpse(mental_health_dataset)
summary.data.frame(mental_health_dataset)
names(mental_health_dataset)
?mental_health_dataset

# I have a couple of questions I want to ask about each filtered version of
# the data set so there is going to be a few filtered data set available
# this is the first.
# question one, how does the averages of just the men compare to the whole
# overall data set in terms of how they compare to the data set overall
men_health <- filter(mental_health_dataset, Gender == "Male")
View(men_health)

# question two, is overall mental health better or worse for those who are
# younger/just starting out in the work force?
young_adult_mental_health <- filter(mental_health_dataset, 18 < Age & Age <= 30)
View(young_adult_mental_health)

# question three, how does a single first world country fair in terms of
# mental health compared to the overall data set?
usa_mental_health <- filter(mental_health_dataset, Country == "USA")
view(usa_mental_health)

# question four, does gender play a role in overall stress levels regardless
# of career path, hours slept, and physical activity being relatively the same?
mental_health_dataset %>% 
  group_by(Gender) %>% 
  summarise(mean(Physical_Activity_Hours),
            mean(Work_Hours))

# question five, does the occupation you have affect how much you are able to
# participated in physical activity?
mental_health_dataset %>% 
  group_by(Occupation) %>% 
  summarise(mean(Physical_Activity_Hours),
            mean(Work_Hours))
ggplot(mental_health_dataset, aes(x = Sleep_Hours,
                                  y = Work_Hours, colour = Stress_Level)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2")

#  a scatter plot only looking at people from 18-30
ggplot(young_adult_mental_health, aes(x = Sleep_Hours,
                                  y = Work_Hours, colour = Occupation)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2")

# I did some testing and I still don't know what to do with this set. I am
# not even sure I know how to explain what I was thinking when I was trying
# to make this work...
mental_health_dataset %>% 
  group_by(Country) %>% 
  group_by(Age) %>% 
  summarize(quantile(mental_health_dataset$Sleep_Hours))

# Seeing that the mid point for the two data points was intriguing. I
# personally think that this data isn't that great because I would have at
# least expected the 50 percentile to hover around 40 hours not 55
quantile(mental_health_dataset$Age, c(.5))
quantile(mental_health_dataset$Work_Hours, c(.5))

for (row in mental_health_dataset)
{
  if (mental_health_dataset[row,3] != "Male" | mental_health_dataset[row,3] != "Female")
  {
    mental_health_dataset <- mental_health_dataset[-c(row),]
  }
}
