# Get the data from life expectancy dataset and omit the rows with NA value
library(hydroGOF)
library(e1071)
library(glmnet)
library(dplyr) 
library(kernlab)
library(randomForest)

life_expectancy_ds <- read.csv(file = "./Life_Expectancy_Data.csv")
life_expectancy_ds <- na.omit(life_expectancy_ds)

# Remove Columns with String Value
life_expectancy_ds_wo_str <- life_expectancy_ds[, 4:22]

# Include Year
Year <- life_expectancy_ds[, 2]
data_with_year <- data.frame(life_expectancy_ds_wo_str, Year)

# Use VIP to see the correlation between life expectancy
std_desc <- as.data.frame(scale(data_with_year))
pca <- prcomp(std_desc)
plot(pca$sdev)
pca$sdev
sum(pca$sdev)
sum(pca$sdev[1:14]) / sum(pca$sdev)

loads <- pca$rotation
loads_vip<-loads[,1:14]
property_vip<-loads_vip[1,]
features_vip<-loads_vip[2:20,]
weight_vip<-property_vip*features_vip
vip <- weight_vip[, 1] + weight_vip[, 2] + weight_vip[, 3] + weight_vip[, 4] + weight_vip[, 5] + weight_vip[, 6] + weight_vip[, 7] + weight_vip[, 8] + weight_vip[, 9] + weight_vip[, 10] + weight_vip[, 11] + weight_vip[, 12] + weight_vip[, 13] + weight_vip[, 14]

# Total.expenditure, BMI, under.five.deaths, Polio, Population, Schooling
sort(x = vip,decreasing = TRUE)
barplot(vip)

set.seed(508)
ds_sort <- sample(1:nrow(data_with_year),nrow(data_with_year)*0.8)

# SLR
train_slr<-data_with_year[ds_sort,]
test_slr<-data_with_year[-ds_sort,]
mdl_slr <- lm(Life.expectancy~Year, data=train_slr)
pred_train_slr <- predict(mdl_slr,data=train_slr)
pred_test_slr <- predict(mdl_slr,newdata=test_slr)
rmse_slr_train <- rmse(pred_train_slr,train_slr$Life.expectancy)
rmse_slr_test <- rmse(pred_test_slr,test_slr$Life.expectancy)
rmse_slr_train
rmse_slr_test
sst <- sum((train_slr$Life.expectancy-mean(train_slr$Life.expectancy))^2)
sse <- sum((pred_train_slr-train_slr$Life.expectancy)^2)
rsq <- 1-sse/sst
rsq
summary(mdl_slr)

# MLR
train_mlr<-data_with_year[ds_sort,]
test_mlr<-data_with_year[-ds_sort,]
mdl_mlr <- lm(Life.expectancy~Total.expenditure+BMI+under.five.deaths+Polio+Population+Schooling, data=train_mlr)
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
summary(mdl_mlr)

# SVR
train_svr<-data_with_year[ds_sort,]
test_svr<-data_with_year[-ds_sort,]
train_svr_d<-data.frame(train_svr)
descriptors_train_svr<-train_svr[,! names(train_svr) %in% c("Life.expectancy")]
descriptors_test_svr<-test_svr[,! names(test_svr) %in% c("Life.expectancy")]
descriptors_train_svr<-as.matrix(descriptors_train_svr)
descriptors_test_svr<-as.data.frame(descriptors_test_svr)
prop_train_svr<-train_svr$Life.expectancy
prop_test_svr<-test_svr$Life.expectancy
mdl_svr<-tune(svm,prop_train_svr~descriptors_train_svr,ranges=list(epsilon=seq(0,1,0.1),cost=1:10))
BstModel<-mdl_svr$best.model
summary(BstModel)
#Update the regression model with the selections from BstModel (kernel, cost, gamma, epsilon)
svmfit <- svm(train_svr$Life.expectancy ~., data = train_svr, method="eps-regression",kernel = 'radial', cost = 6, gamma=0.05263158,epsilon=0,scale=FALSE)
pred_train_svr<-predict(svmfit, data=descriptors_train_svr)
pred_test_svr<-predict(svmfit,newdata=descriptors_test_svr)
rmse_SVR_train<-rmse(pred_train_svr,prop_train_svr)
rmse_SVR_test<-rmse(pred_test_svr,prop_test_svr)
rmse_SVR_train
rmse_SVR_test
sst<-sum((train_svr$Life.expectancy-mean(train_svr$Life.expectancy))^2)
sse<-sum((pred_train_svr-train_svr$Life.expectancy)^2)
rsq<-1-sse/sst
rsq

# GPR
train_gpr<-data_with_year[ds_sort,]
test_gpr<-data_with_year[-ds_sort,]
descriptors_train_gpr<-train_gpr[,! names(train_gpr) %in% c("Life.expectancy")]
descriptors_test_gpr<-test_gpr[,! names(train_gpr) %in% c("Life.expectancy")]
mdl_gpr<-gausspr(descriptors_train_gpr,train_gpr$Life.expectancy)
pred_train_gpr<-predict(mdl_gpr,descriptors_train_gpr)
pred_test_gpr<-predict(mdl_gpr,descriptors_test_gpr)
rmse_gpr_train<-rmse(pred_train_gpr,as.matrix(train_gpr$Life.expectancy))
rmse_gpr_test<-rmse(pred_test_gpr,as.matrix(test_gpr$Life.expectancy))
rmse_gpr_train
rmse_gpr_test
sst<-sum((train_gpr$Life.expectancy-mean(train_gpr$Life.expectancy))^2)
sse<-sum((pred_train_gpr-train_gpr$Life.expectancy)^2)
rsq<-1-sse/sst
rsq

# Random Forest
train_rf<-data_with_year[ds_sort,]
test_rf<-data_with_year[-ds_sort,]
model_rf<-randomForest(train_rf$Life.expectancy~.,data=train_rf,mtry=3,importance=TRUE,na.action=na.omit)
pred_train_rf<-predict(model_rf,train_rf)
pred_test_rf<-predict(model_rf,newdata=test_rf)
rmse_rf_train<-rmse(pred_train_rf,train_rf$Life.expectancy)
rmse_rf_test<-rmse(pred_test_rf,test_rf$Life.expectancy)
rmse_rf_train
rmse_rf_test
sst<-sum((train_rf$Life.expectancy-mean(train_rf$Life.expectancy))^2)
sse<-sum((pred_train_rf-train_rf$Life.expectancy)^2)
rsq<-1-sse/sst
rsq

