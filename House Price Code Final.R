#get required packages
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(moments)
library(glmnet)
library(elasticnet)
library(knitr)
library(mlbench)
library(car)
library(corrplot)

#下面三行不知道在干啥2333
options(width=100)
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)
ROOT.DIR <- "/Users/zhoujiawang/Desktop/BigData Final/house-prices-advanced-regression-techniques"  
#facilitates testing inside and outside of Rmarkdown notebook
# import the data
train <- read.csv("E:/BRANDEIS/2nd semester class/Big data/project/house price/material/train.csv",stringsAsFactors = FALSE)
test <- read.csv("E:/BRANDEIS/2nd semester class/Big data/project/house price/material/test.csv",stringsAsFactors = FALSE)
# show the first few rows of training data
# 81 different variables in total 这也太多???2333
head(train)
#select desired variables
train$FullBath=train$FullBath+train$BsmtFullBath
train$HalfBath=train$HalfBath+train$BsmtHalfBath
test$FullBath=test$FullBath+test$BsmtFullBath
test$HalfBath=test$HalfBath+test$BsmtHalfBath
names(train)
train<-
  train %>%
  select(SalePrice,LotFrontage,LotArea,YearRemodAdd,MasVnrArea,BsmtQual,TotalBsmtSF,Fireplaces,
         CentralAir,GrLivArea,FullBath,HalfBath,BedroomAbvGr,KitchenAbvGr,GarageArea,
         PavedDrive,Neighborhood,OverallQual)
test<-
  test %>%
  select(LotFrontage,LotArea,YearRemodAdd,MasVnrArea,BsmtQual,TotalBsmtSF,Fireplaces,
         CentralAir,GrLivArea,FullBath,HalfBath,BedroomAbvGr,KitchenAbvGr,GarageArea,
         PavedDrive,Neighborhood,OverallQual)

#描述性统�?
#sort on decreasing correlations with SalePrice
set.seed(7)
numericVars <- names(which(sapply(train, is.numeric))) #index vector numeric variables
catagoricalVars <- names(which(sapply(train, is.character)))#index vector catagorical
all_numVar <- train[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
#GrLivArea Plot
ggplot(data=train[!is.na(train$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))
#location Plot
ggplot(train, aes(x=Neighborhood, y=SalePrice, color=Neighborhood)) +
  geom_boxplot()
ggplot(data=train, aes(x=Neighborhood)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Overall Quality Plot
ggplot(data=train, aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))

#Combine the data first for the analysis
all_data <- rbind(select(train,LotFrontage:OverallQual),select(test,LotFrontage:OverallQual))

# get data frame of SalePrice and log(SalePrice + 1) for plotting
df <- rbind(data.frame(version="log(price+1)",x=log(train$SalePrice + 1)),
            data.frame(version="price",x=train$SalePrice))
# plot histogram
ggplot(data=df) +
  facet_wrap(~version,ncol=2,scales="free_x") +
  geom_histogram(aes(x=x))

#Log Transformation
train$SalePrice <- log(train$SalePrice + 1)
# for numeric feature with excessive skewness, perform log transformation
# first get data type for each feature
feature_classes <- sapply(names(all_data),function(x){class(all_data[[x]])})
numeric_feats <-names(feature_classes[feature_classes != "character"])
# determine skew for each numeric feature
skewed_feats <- sapply(numeric_feats,function(x){skewness(all_data[[x]],na.rm=TRUE)})
# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[skewed_feats > 0.75]
# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  all_data[[x]] <- log(all_data[[x]] + 1)
}

#Catagorical_feats
categorical_feats <- names(feature_classes[feature_classes == "character"])
# use caret dummyVars function for hot one encoding for categorical features
dummies <- dummyVars(~.,all_data[categorical_feats])
categorical_1_hot <- predict(dummies,all_data[categorical_feats])
categorical_1_hot[is.na(categorical_1_hot)] <- 0  #for any level that was NA, set to zero

#for any missing values in numeric features, impute mean of that feature
numeric_df <- all_data[numeric_feats]
for (x in numeric_feats) {
  mean_value <- mean(train[[x]],na.rm = TRUE)
  all_data[[x]][is.na(all_data[[x]])] <- mean_value
}

# reconstruct all_data with pre-processed data
all_data <- cbind(all_data[numeric_feats],categorical_1_hot)

# create data for training and test
X_train1 <- all_data[1:nrow(train),]
X_test <- all_data[(nrow(train)+1):nrow(all_data),]
X_train<-data.frame(X_train1,train$SalePrice)



#Model Selection
#Code of GLM,LM,SVM,CART and knn methods are found in "Longley" in Namini's CaseStudies fold
control <- trainControl(method="repeatedcv",number=10,repeats=3,verboseIter=FALSE)
#GLM
set.seed (7)
#A question here is that I don't know what set.seed means. lol
fit.glm <- train(train.SalePrice~., data=X_train, method="glm", metric="RMSE", trControl=control)
print(fit.glm)
#SVM
set.seed(7)
grid <- expand.grid(.sigma=c(0.01,0.05,0.1), .C=c(1))
fit.svm <- train(train.SalePrice~., data=X_train, method="svmRadial", metric="RMSE", tuneGrid=grid, trControl=control)
print(fit.svm)
#CART
set.seed(7)
grid <- expand.grid(.cp=c(0.01,0.05,0.1))
fit.cart <- train(train.SalePrice~., data=X_train, method="rpart", metric="RMSE", tuneGrid=grid, trControl=control)
print(fit.cart)
#kNN
set.seed(7)
grid <- expand.grid(.k=c(1,3,5,7))
fit.knn <- train(train.SalePrice~., data=X_train, method="knn", metric="RMSE", tuneGrid=grid, trControl=control)
print(fit.knn)

#test out Ridge regression model
lambdas <- seq(1,0,-0.001)
set.seed(7)  # for reproducibility
model_ridge <- train(x=X_train1,y=X_train$train.SalePrice,method="glmnet",metric="RMSE",maximize=FALSE,trControl=control,
                     tuneGrid=expand.grid(alpha=0,lambda=lambdas))
mean(model_ridge$resample$RMSE)

#test out lasso regression model
set.seed(7)  # for reproducibility
model_lasso <- train(x=X_train1,y=X_train$train.SalePrice,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=control,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
mean(model_lasso$resample$RMSE)
#The mean RMSE of the lasso model is larger than the ridge model's. Therefore, the ridge model is better?

#Compare algorithms
results <- resamples(list(SVM=fit.svm, CART=fit.cart, kNN=fit.knn, GLM=fit.glm, RIDGE=model_ridge,LASSO=model_lasso))
summary(results)
dotplot(results)

#Coef
coef <- data.frame(coef.name = dimnames(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda))[[1]], 
                   coef.value = matrix(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda)))
coef <- coef[-1,]
picked_features <- nrow(filter(coef,coef.value!=0))
not_picked_features <- nrow(filter(coef,coef.value==0))
cat("Lasso picked",picked_features,"variables and eliminated the other",
    not_picked_features,"variables\n")

#Accuracy of prediction
prediction<-predict(fit.glm,newdata=X_train1)
X_train1<-data.frame(X_train,prediction)
ggplot(data=X_train1,aes(x=prediction,y=train.SalePrice)) + geom_point() +  geom_abline(intercept = 0, slope = 1, color="red")

#Prediction with test dataset
prediction1<-predict(fit.glm,newdata=X_test)
