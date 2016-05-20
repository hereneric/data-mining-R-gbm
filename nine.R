#install.packages("gbm",  repos = "http://cran.us.r-project.org");
rm(list=ls())
setwd("/Users/herenvictor/Desktop/stats315B/HW#2/code")
library(gbm)
set.seed(100);
wholeData = read.table("Occupation_Data.txt", sep = ",")
colnames(wholeData) <- c("occupation","housing_type","sex","marital_status","age","education","annual_income","time",
"dual_income","family","underage","householder","ethnicity","language")
for (i in c(1,2,3,4,9,12,13,14)) {
	wholeData[, i] <- as.factor(wholeData[, i]);	
}
set.seed(200);
sample=sample(nrow(wholeData), round(nrow(wholeData) * 0.8));
train = wholeData[sample,];
test = wholeData[-sample,];
gbm1 <- gbm(occupation~., data=train, train.fraction=1, interaction.depth=4, shrinkage=.05, n.trees=1000, bag.fraction=0.5, cv.folds=5, distribution="multinomial", verbose=T);
best.iter = gbm.perf(gbm1, method="cv");
prediction <- predict(gbm1, test, type = "response");
result = rep(NA, nrow(test));
for (i in 1:length(result)){
	chance = prediction[i,,];
	chance.max = max(chance);
	result[i] = which(chance == chance.max);
}
error.overall = sum(result != test[, 1]) / length(result);
error = rep(0,9);
for (i in 1:9) {
	index = which(test[, 1] == i);
	error[i] = sum(result[index] != i) / length(index);
}
error
error.overall