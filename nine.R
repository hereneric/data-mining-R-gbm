#install.packages("gbm",  repos = "http://cran.us.r-project.org");
rm(list=ls())
setwd("/Users/herenvictor/Desktop/stats315B/HW#2/code")
library(gbm)
set.seed(1);
occData = read.table("Occupation_Data.txt", sep = ",")
colnames(occData) <- c("occupation","housing_type","sex","marital_status","age","education","annual_income","time",
"dual_income","family","underage","householder","ethnicity","language")
# mark the categorical variables
for (i in c(1,2,3,4,9,12,13,14)) {
	occData[, i] <- as.factor(occData[, i]);	
}
# occData$occupation <- as.factor(occData$occupation);
# occData$housing_type <- as.factor(occData$housing_type);
# occData$sex <- as.factor(occData$sex);
# occData$marital_status <- as.factor(occData$marital_status);
# occData$dual_income <- as.factor(occData$dual_income);
# occData$householder <- as.factor(occData$householder);
# occData$ethnicity <- as.factor(occData$ethnicity);
# occData$language <- as.factor(occData$language);
set.seed(2);
sample=sample(nrow(occData), round(nrow(occData) * 0.8));
train = occData[sample,];
test = occData[-sample,];
gbm1 <- gbm(occupation~., data=train, train.fraction=1, interaction.depth=4, shrinkage=.05, n.trees=1000, bag.fraction=0.5, cv.folds=5, distribution="multinomial", verbose=T);
best.iter = gbm.perf(gbm1, method="cv");
prediction <- predict(gbm1, test, type = "response");
prediction.result = rep(NA,dim(test)[1]);
for (i in 1:length(prediction.result)){
	tem = prediction[i,,];
	tem.max = max(tem);
	prediction.result[i] = which(tem == tem.max);
}
error.overall = sum(prediction.result != test$occupation) / length(prediction.result);
error = rep(0,9);
for (i in 1:9) {
	index = which(test$occupation == i);
	error[i] = sum(prediction.result[index] != i) / length(index);
}
error
error.overall
