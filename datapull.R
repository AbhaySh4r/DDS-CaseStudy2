# General Script connecting to Smuddsproject2 bucket
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAXNDHFDN3FSMJBJF5",
"AWS_SECRET_ACCESS_KEY" = "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", "AWS_DEFAULT_REGION" = "us-east-2")


library(aws.s3)
library(readxl)
library(ggplot2)
library(caret)
library(RCurl)
library(class)
library(tidyverse)
library(olsrr)
## Correlation
install.packages("corrplot")
library(corrplot)

## Pulling the Data
bucket_list()
get_bucket("smuddsproject2")

fullset = s3read_using(FUN = read.csv, bucket = "smuddsproject2", object = "CaseStudy2-data.csv")
NoSal <- s3read_using(FUN = read_xlsx, object = "CaseStudy2CompSet No Salary.xlsx", bucket = "smuddsproject2")
CompSetAttr <- s3read_using(FUN = read.csv, object = "CaseStudy2CompSet No Attrition.csv", bucket = "smuddsproject2")

## Organizing Data

numericset <- fullset %>% select(where(is_numeric))
fullset$Attrition = as.factor(fullset$Attrition)

selected_features = numericset %>% select("Age", "MonthlyRate", "DistanceFromHome", "JobSatisfaction", "WorkLifeBalance", "YearsAtCompany")
selected_features$DistanceFromHome = as.factor(selected_features$DistanceFromHome)
selected_features$WorkLifeBalance = as.factor(selected_features$WorkLifeBalance)


## Generating a linear model with stepwise feature  ( Attrition ): 

character_parameters = fullset %>% select_if(is.character) %>% names()

fullset_conversion = fullset
fullset_conversion[character_parameters] = lapply(fullset_conversion[character_parameters], factor)
fullset_conversion = subset(fullset_conversion, select = -Over18) #rm over 18, one level
fullset_conversion$MonthlyRate = log(fullset_conversion$MonthlyRate)

characterset = fullset_conversion %>% select_if(is.factor) %>% names()
fullset_conversion[characterset] = lapply(fullset_conversion[characterset], as.numeric)
corrplot(cor(fullset_conversion), tl.cex = 0.5)
fullset_conversion = subset(fullset_conversion, select = -c(EmployeeCount, StandardHours, ID, EmployeeNumber))
corrplot(cor(fullset_conversion), tl.cex = 0.5)
#remove aliased coeef. (joblevel, employeecount, standardhours, performancerating, totalworkingyears, yearsatcurrentrole
conversion_sig = findCorrelation(x = cor(fullset_conversion), cutoff = 0.4)

parameter_fit = lm(Attrition~., data = fullset_conversion)

stepwise_selection = ols_step_both_p(parameter_fit, pent = 0.4, prem = 0.1, details = FALSE) 
summary(stepwise_selection$model)

control = trainControl(method = "repeatedcv", number = 10, repeats = 10)
model <- train(MonthlyRate~. , data = fullset_conversion, method = "lvq", preProcess= "scale", trControl = control)

importance = varImp(model, scale = FALSE)
plot(importance)



## Downsample to resolve class imbalance: 

features_down = downSample(numericset, fullset$Attrition, yname = "Attrition") #downsample
train_down = subset(features_down, select = -c(Attrition))

features_up = upSample(selected_features, fullset$Attrition, yname = "Attrition") #upsample
train_up = subset(features_up, select = -c(Attrition))

## KNN Model Generation w/ DownSampling


#train = subset(fullset, select = -c(Attrition))


knn_model = knn.cv(train_up, features_up$Attrition, k = 3, prob = TRUE)
confusionMatrix(table(knn_model, features_up$Attrition), positive = "Yes")

# Comp-Set KNN Model

CompSetAttrFeatures = CompSetAttr %>% select("Age", "MonthlyRate",
                                             "DistanceFromHome", "JobSatisfaction",
                                             "WorkLifeBalance", "YearsAtCompany")

knn(train_up, CompSetAttrFeatures, features_up$Attrition, k = 3)




## Linear Model Generation

filtered_numset <- numericset %>% select(-c(EmployeeCount, StandardHours))
cor(filtered_numset)
corrplot(cor(filtered_numset), tl.cex = 0.6, method = 'ellipse', order = 'AOE', type = 'lower')

fullset$Attrition = as.factor(fullset$Attrition)
fullset %>% ggplot(aes(y = (MonthlyIncome), x = Education*JobLevel, color = fullset[,'Attrition'])) + geom_point() + geom_smooth(method = "lm", formula = y~poly(x,2))

fit = lm(MonthlyIncome ~ TotalWorkingYears + StockOptionLevel + YearsWithCurrManager + Age, data = numericset)
badfit = lm(MonthlyIncome ~ ., data = fullset_conversion)
# include class based income (maybe attrition/jobstatus)
plot(fit)
plot(badfit)




## KNN External CrossValidation: 

choice = c("MonthlyIncome", "TotalWorkingYears", "StockOptionLevel", "YearsWithCurrManager", "Age")

sample_size = 100
test_index = sample(seq(1:dim(fullset)[1]), sample_size)

AttrTrain = fullset[-test_index,]
AttrTrainLabels = AttrTrain %>% select("Attrition")
AttrTrain = AttrTrain %>% select(all_of(choice))
AttrTest = fullset[test_index,]
AttrTestLabels = AttrTest %>% select("Attrition")
AttrTest = AttrTest %>% select(all_of(choice))


AttrTrainNum = AttrTrain %>% select(where(is_numeric)) 
AttrTrainNum = AttrTrainNum %>% select(all_of(choice))

ext_features_up = upSample(AttrTrainNum, as.factor(AttrTrainLabels$Attrition), yname = "Attrition")
ext_train_up = subset(ext_features_up, select = -c(Attrition))

ext_knn = knn(ext_train_up, AttrTest, ext_features_up$Attrition, k = 2)
confusionMatrix(table(ext_knn, as.factor(AttrTestLabels$Attrition)), positive = "Yes")

## NaiveBayes Model
library(klaR)


badmeme = subset(fullset, select = -c(EmployeeCount, StandardHours, BusinessTravel, Department, EducationField, Gender, JobRole, MaritalStatus, Over18, OverTime))

control = trainControl(method = "repeatedcv", number = 10, repeats = 10)

nb_model <- train(Attrition ~ ., data = ext_features_up, method = "nb", trControl = control)

nb_prediction = predict(nb_model, ext_train_up)

confusionMatrix(nb_prediction, ext_features_up$Attrition)

## Feature Importance: 

install.packages("mlbench")
library(mlbench)

corr_dataset = subset(fullset, select = -c(Attrition, EmployeeCount, StandardHours))
corr_dataset = corr_dataset %>% select_if(is.numeric)


sigdata = findCorrelation(x = cor(corr_dataset), cutoff = 0.3) #reduce multicollinearity

head(corr_dataset[sigdata])

summary(fullset)
badmeme = subset(fullset, select = -c(EmployeeCount, StandardHours, BusinessTravel, Department, EducationField, Gender, JobRole, MaritalStatus, Over18, OverTime))

control = trainControl(method = "repeatedcv", number = 10, repeats = 10)
model <- train(Attrition ~. , data = badmeme, method = "lvq", preProcess= "scale", trControl = control)

importance = varImp(model, scale = FALSE)
plot(importance)
