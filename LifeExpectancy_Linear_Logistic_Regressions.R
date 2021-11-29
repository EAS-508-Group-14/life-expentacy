## Load all required libraries for cleaning and analysis
library(dplyr)
library(stats)
library(e1071)
library(ggplot2)
library(forecast)
library(psych)
library(tidyr)
library(ExcelFunctionsR)
library(aod)
library(tidyverse)
library(modelr)
library(broom)

#Part1 - Structure, Dimension and Summary of the Dataset

#Data reading
data <- read.csv('./Life Expectancy Data.csv');
#Compactly Display the Structure of an Arbitrary R Object
str(data);
dim(data); #Dimensions of an object
summary(data) #Summary of the given data

#Part 2 - Collection and Cleaning Data

# Make sure all the countries have data for all years (2000-2015)
# Count number of country
unique(data$Country)
unique(data$Year)

## Normally, with 193 countries and and 16 years, we should have 
193 * 16  ## 3088 rows, but instead we have

length(data$Country) ##2938 rows, that's
countrycount <- data.frame(table(data$Country))  #countries without 16 counts will be excluded from the data set 

# cleaning out N/A 
is.na(data)
sum(is.na(data))
prelimlifexpect <- na.omit(data)

#filter final data set to be processed
flifexpect <- left_join(prelimlifexpect, countrycount, by = c("Country" = "Var1"))
sum(is.na(flifexpect))
head(flifexpect)

# Part 3 - Regression Analysis
## Correlation expenditure vs LifeExpectancy
par(mar=c(2,2,2,2))
plot(flifexpect$Total.expenditure, flifexpect$Life.expectancy,xlab="Total.expenditure",ylab = "Life.expectancy")

corr.test(flifexpect$Total.expenditure, flifexpect$Life.expectancy)
corr.test(data$Total.expenditure, data$Life.expectancy)
# Comments: the correlation of 0.22 implies there is no strong correlation between healthcare expenditure and life expectancy
# in other words, life expectancy doesn't depend much on how much is spent on healthcare.


# Linear regression
y <-  flifexpect$Life.expectancy
x <- flifexpect$Total.expenditure

rmod <- lm(y ~ x)

summary(rmod)
attributes(rmod)
median(flifexpect$Life.expectancy, na.rm = TRUE)
median(flifexpect$Total.expenditure, na.rm = TRUE)
plot(flifexpect$Total.expenditure, flifexpect$Life.expectancy,xlab="Total.expenditure",ylab = "Life.expectancy",main = "All Countries")
abline(lm(y~x))
# From this plot, we can see that as the expenditure gets above the median, so does the life expectancy.
# There might be some skewness in the data that prohibit the establishment of a strong trend. 
# We will classify data based on country status: Developing and Developed


# Classification
# Determining the country status by looking at life expectancy

age_group <- group_by(flifexpect, flifexpect$Life.expectancy, na.rm = TRUE)
summarise(age_group,
          avg = mean(flifexpect$Status),
          median = median(flifexpect$Status),
          n = n())
write.csv(age_group, "age_group.csv")
# The average life expectancy (AVERAGEIF in Excel) gives the following results
# For Developing countries, the average life expectancy is 66.30 years
# For Developed countries, the average life expectancy is 79.65 years.

#Developing countries
dvping <- filter(age_group, Status == "Developing")

ydeving <- dvping$Life.expectancy
xdeving <- dvping$Total.expenditure
rmoddeving <- lm(ydeving ~ xdeving)
summary(rmoddeving)
plot(xdeving, ydeving,xlab="Total.expenditure",ylab = "Life.expectancy",main = "Developing Countries",, col='blue')
abline(lm(ydeving ~ xdeving))

mean(dvping$Total.expenditure, na.rm = TRUE)
mean(dvping$Life.expectancy, na.rm = TRUE)

corr.test(dvping$Total.expenditure, dvping$Life.expectancy)
median(dvping$Total.expenditure, na.rm = TRUE)
median(dvping$Life.expectancy, na.rm = TRUE)

# Comments: At 4.92%, the p-value is still below 5% but it's way higher than the p-value for all countries data set.
# Hence still falling to confirm a dependence between total expenditure and life expectancy
# However, we observe that as the total expenditure cross its median value so does the life expectancy

#Developed Countries

dvped <- filter(age_group, Status == "Developed")

ydvped <- dvped$Life.expectancy
xdvped <- dvped$Total.expenditure
rmoddvped <- lm(ydvped ~ xdvped)
summary(rmoddvped)
plot(xdvped, ydvped,xlab="Total.expenditure",ylab = "Life.expectancy",main = "Developed Countries",col='red')
abline(lm(ydvped ~ xdvped))

mean(dvped$Total.expenditure, na.rm = TRUE)
mean(dvped$Life.expectancy, na.rm = TRUE)
corr.test(dvped$Total.expenditure, dvped$Life.expectancy)
median(dvped$Total.expenditure, na.rm = TRUE)
median(dvped$Life.expectancy, na.rm = TRUE)
# Comments: Developed countries spend more than developing countries; developed countries also have a higher life expectancy.

#Part 4 - Classification Analysis

#Brief summary of the cleaned data frame
head(flifexpect)

#Binary response (outcome, dependent) variable is Status
#Status - Developed or Developing status
#Predictor variable is Life Expectancy, GDP, Income Composition, Total Expenditure
#Life Expectancy - Life expectancy in Age
#Income Composition - Human Development Index in terms of income composition of resources (index ranging from 0 to 1)
#Total Expenditure - General government expenditure on health as a percentage of total government expenditure

#Prepare the data frame for the model
#Say for the year 2014
logdf <- flifexpect[flifexpect$Year == 2014, ]
row.names(logdf) <- logdf$Country
colnames(logdf)

#Subset the data frame for the classification model
logdf <- subset(logdf,select = c(Status, Life.expectancy,Income.composition.of.resources,Total.expenditure))
glimpse(logdf)

#Convert the status to binary
lookup <- c("Developing"=0, "Developed"=1)
logdf$binStatus <- lookup[logdf$Status]
glimpse(logdf)

#Drop the string Status column from data frame
logdf <- subset(logdf, select = -c(Status))

#Rename the binary status column to Status
names(logdf)[names(logdf) == "binStatus"] <- "Status"
glimpse(logdf)

#Rank each country by income composition of resources
#Wikipedia on HDI
#A value above 0.800 is classified as very high, 
#between 0.700 and 0.799 high, 
#0.550 to 0.699 as medium 
#and anything below 0.550 as low

#Group each Income Composition into Rank Range
logdft <- logdf %>% mutate(HDI.Rank = cut(logdf$Income.composition.of.resources, c(0, 0.550, 0.700, 0.800, Inf)))

#Convert the HDI Rank to rank 1-4
# 1 - 0.800 and above - Very High
# 2 - 0.700 to 0.799 - High
# 3 - 0.550 to 0.699 - Medium
# 4 - 0.550 and below - Low
lookup2 <- c("(0,0.55]"=4, "(0.55,0.7]"=3, "(0.7,0.8]"=2, "(0.8,Inf]"=1)
logdft$ranked.HDI <- lookup2[logdft$HDI.Rank]

#Drop Income Composition and the HDI Rank column from data frame
#As it represented by ranked.HDI
logdft <- subset(logdft, select = -c(Income.composition.of.resources,HDI.Rank))

glimpse(logdft)

#Show summary of the data frame to ensure its good for logit model
summary(logdft)

#To predict the outcome of Status based on Life Expectancy value

# Logistics Regression
# Split the data into two chunks; training and testing set
# Regression is done on training set 
# total rows for 2014 year are 131. Divide these into train and test set
train <- logdft[1:65,]
test <- logdft[65:131,]

# Simple Logistic Regression
model1 <- glm(Status ~ Life.expectancy, family = "binomial", data = train)

# Summary of the model
summary(model1)

ggplot(logdft, aes(x=Life.expectancy, y=Status)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) + ggtitle("Logistic Regression for Country status given Life Expectancy in 2014 year")

# Accessing coefficients
# Shows the coefficient estimates and related information 
# that result from fitting a logistic regression model 
# in order to predict the probability of Developed Status = Yes using Life Expectancy
tidy(model1)

# Interpretation of the balance coefficient 
# for every increasing of life expectancy value by 0.1
# the odds of the Status of a country increases by a factor of 1.3577
exp(coef(model1))

# Making prediction
# From the coefficients calculated, to compute the probability of
# Country's Status for any given Life Expectancy value
# For example, given Life Expectancy are 68.9 and 73.7 
predict(model1, data.frame(Life.expectancy = c(68.9, 73.7)), type = "response")

# From the result, 
# for Country with Life Expectancy of 68.9, 
# it is ~13.73% probability that it is a developed country
# However for Country with Life Expectancy of 73.7,
# it is more than 56.9% likely is a developed country,
# which is a high jump from mere 68.9 to 73.7

