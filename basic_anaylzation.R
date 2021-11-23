# Get the data from life expectancy dataset and omit the rows with NA value
library(hydroGOF)
library(e1071)
library(glmnet)
library(dplyr) 

life_expectancy_ds <- read.csv(file = "./Life_Expectancy_Data.csv")
life_expectancy_ds <- na.omit(life_expectancy_ds)

# Remove Columns with String Value
life_expectancy_ds_wo_str <- life_expectancy_ds[, 4:22]

# Divide dataset to Developed Country and Developing Country
developing <- filter(life_expectancy_ds, Status == "Developing")
developed <- filter(life_expectancy_ds, Status == "Developed")

# Data that only contains economy factors descriptors
economy <- life_expectancy_ds[, c("Life.expectancy", "GDP", "thinness..1.19.years", "thinness.5.9.years", "Income.composition.of.resources")]

# Data that only contains medical factors descriptors
medical <- life_expectancy_ds[, c("Life.expectancy", "Hepatitis.B", "Polio", "Total.expenditure", "Diphtheria")]

# Data that only contains disease factors descriptors
disease <- life_expectancy_ds[, c("Life.expectancy", "Measles", "HIV.AIDS")]

# Data by recent years (2015 only has two rows so we exclude it)
fourteen <- filter(life_expectancy_ds, Year == "2014")
thirdteen <- filter(life_expectancy_ds, Year == "2013")
twelve <- filter(life_expectancy_ds, Year == "2012")
eleven <- filter(life_expectancy_ds, Year == "2011")
ten <- filter(life_expectancy_ds, Year == "2010")

life_expectancy <- life_expectancy_ds_wo_str[,1]
descriptors <- life_expectancy_ds[, 5:22]


# Use VIP to see the correlation between life expectancy
std_desc <- as.data.frame(scale(life_expectancy_ds_wo_str))
pca <- prcomp(std_desc)
plot(pca$sdev)
pca$sdev
sum(pca$sdev)
sum(pca$sdev[1:13]) / sum(pca$sdev)

loads <- pca$rotation
loads_vip<-loads[,1:13]
property_vip<-loads_vip[1,]
features_vip<-loads_vip[2:19,]
weight_vip<-property_vip*features_vip
vip <- weight_vip[, 1] + weight_vip[, 2] + weight_vip[, 3] + weight_vip[, 4] + weight_vip[, 5] + weight_vip[, 6] + weight_vip[, 7] + weight_vip[, 8] + weight_vip[, 9] + weight_vip[, 10] + weight_vip[, 11] + weight_vip[, 12] + weight_vip[, 13]

# Adult.Mortality, Polio, under.five.deaths, Measles, Population
sort(x = vip,decreasing = TRUE)
barplot(vip)


set.seed(508)
ds_sort <- sample(1:nrow(life_expectancy_ds_wo_str),nrow(life_expectancy_ds_wo_str)*0.8)

# MLR without reduced descriptors
train_mlr <- life_expectancy_ds_wo_str[ds_sort,]
test_mlr <- life_expectancy_ds_wo_str[-ds_sort,]
mdl_mlr <- lm(train_mlr$Life.expectancy~., data=train_mlr)
pred_train_mlr <- predict(mdl_mlr,data=train_mlr)
pred_test_mlr <- predict(mdl_mlr,newdata=test_mlr)
rmse_mlr_train <- rmse(pred_train_mlr,train_mlr$Life.expectancy)
rmse_mlr_test <- rmse(pred_test_mlr,test_mlr$Life.expectancy)
rmse_mlr_train
rmse_mlr_test
sst <- sum((train_mlr$Life.expectancy-mean(train_mlr$Life.expectancy))^2)
sse <- sum((pred_train_mlr-train_mlr$Life.expectancy)^2)
rsq <- 1-sse/sst
rsq
