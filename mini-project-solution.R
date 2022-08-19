##### 1 Assignment Description #####

# For this project, I have decided to use a dataset on the Happiness Report
# of every country. It contains attributes such as the rank each country
# receives in terms of its happiness along with factors such as the GDP per
# capita, generosity, dystopia, etc.

world_happiness <- read.csv("/Users/rayhan/Documents/School/BU/2) Spring 2022/CS 555 (Data Analysis and Visualization with R)/Project/Dataset/world-happiness-report.csv")

# The purpose of this research is to determine which factors leads to a happy
# country.

##### 2 Research Scenario Description #####

# The purpose of this research is to determine what factor will lead to an
# overall happier population in a country. We will be using both logistic and
# linear regression, to determine which factors will lead to a happier country.

##### 3 Describe the dataset #####

# Link to the dataset: https://worldhappiness.report/

# Reduce the dataset
world_happiness <- subset(world_happiness, select = -c(Positive.affect, Negative.affect))
world_happiness <- na.omit(world_happiness)

### Describe each column ###
### Country name = Name of country that is being analyzed
### year = the year the report was conducted
### Life Ladder = A score received for a country's happiness
### Log GDP per capita = the logged version of GDP per capita
### Social support = The ranking for a country's social support
### Healthy life expectancy at birth = Expected lifespan
### Freedom to make life choices = Freedom index
### Generosity = A metric on how generous the country is
### Perceptions of corruption = How badly the citizens of said country think 
###                             about corruption in the country.

##### 4 Research Question #####

# My main research question would be on which attribute has the most and least
# affect when measuring the happiness of a country. I will then see which of 
# these factors correlate the most to world happiness, and see how a country
# that scored the highest/lowest in that attribute fair in terms 
# of its happiness.

##### 5 Your Solution R code #####

happiness_lm <- lm(Life.Ladder ~ Log.GDP.per.capita + Social.support + Healthy.life.expectancy.at.birth + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data = world_happiness)
summary(happiness_lm)

happiness_anova <- aov(Life.Ladder ~ Log.GDP.per.capita + Social.support + Healthy.life.expectancy.at.birth + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data = world_happiness)
summary(happiness_anova)

# What is the best ranked country?
print(world_happiness$Country.name[[which.max(world_happiness$Life.Ladder)]])
print(world_happiness$year[[which.max(world_happiness$Life.Ladder)]])
print(world_happiness$Life.Ladder[[which.max(world_happiness$Life.Ladder)]])

# What is the worst ranked country?
print(world_happiness$Country.name[[which.min(world_happiness$Life.Ladder)]])
print(world_happiness$year[[which.min(world_happiness$Life.Ladder)]])
print(world_happiness$Life.Ladder[[which.min(world_happiness$Life.Ladder)]])

# Which country in what year ranked best in social score and what is its value.
print(world_happiness$Country.name[[which.max(world_happiness$Social.support)]])
print(world_happiness$year[[which.max(world_happiness$Social.support)]])
print(world_happiness$Social.support[[which.max(world_happiness$Social.support)]])
print(world_happiness$Life.Ladder[[which.max(world_happiness$Social.support)]])

# Which country in what year ranked lowest in social score and value?
print(world_happiness$Country.name[[which.min(world_happiness$Social.support)]])
print(world_happiness$year[[which.min(world_happiness$Social.support)]])
print(world_happiness$Social.support[[which.min(world_happiness$Social.support)]])
print(world_happiness$Life.Ladder[[which.min(world_happiness$Social.support)]])

# Which country ranked most in life expectancy and value?
print(world_happiness$Country.name[[which.max(world_happiness$Healthy.life.expectancy.at.birth)]])
print(world_happiness$Healthy.life.expectancy.at.birth[[which.max(world_happiness$Healthy.life.expectancy.at.birth)]])
print(world_happiness$year[[which.max(world_happiness$Healthy.life.expectancy.at.birth)]])
print(world_happiness$Life.Ladder[[which.max(world_happiness$Healthy.life.expectancy.at.birth)]])

# Which country ranked least in life expectancy and value?
print(world_happiness$Country.name[[which.min(world_happiness$Healthy.life.expectancy.at.birth)]])
print(world_happiness$Healthy.life.expectancy.at.birth[[which.min(world_happiness$Healthy.life.expectancy.at.birth)]])
print(world_happiness$year[[which.min(world_happiness$Healthy.life.expectancy.at.birth)]])
print(world_happiness$Life.Ladder[[which.min(world_happiness$Healthy.life.expectancy.at.birth)]])

# Which country is most corrupt?
print(world_happiness$Country.name[[which.max(world_happiness$Perceptions.of.corruption)]])
print(world_happiness$Perceptions.of.corruption[[which.max(world_happiness$Perceptions.of.corruption)]])
print(world_happiness$year[[which.max(world_happiness$Perceptions.of.corruption)]])
print(world_happiness$Life.Ladder[[which.max(world_happiness$Perceptions.of.corruption)]])

# Which country is least corrput?
print(world_happiness$Country.name[[which.min(world_happiness$Perceptions.of.corruption)]])
print(world_happiness$Perceptions.of.corruption[[which.min(world_happiness$Perceptions.of.corruption)]])
print(world_happiness$year[[which.min(world_happiness$Perceptions.of.corruption)]])
print(world_happiness$Life.Ladder[[which.min(world_happiness$Perceptions.of.corruption)]])

##### 6 State your conclusion #####

# It looks like social support is the factor that correlates the most positively
# to the happiness of the world, for every 0.1 increase in social support, the 
# happiness score for said country increases by 0.23. The least positively 
# correlating feature would be Life expectancy only increasing the happiness
# score by 0.04 for every additional 10 years in life expectancy.


