library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart)
#install.packages("caret")
library(caret)


setwd("C:/2021/IDS 472/homework 3")
data <- read.csv("covid_19_dataset.csv")

#questions 1.a, filter covid_results for just positive
question_1 <- data %>% filter(corona_result == "positive")
nrow(question_1)

#question 1.b, clean data
question_2 <- data %>% filter(corona_result == "positive" | corona_result == "negative")
question_2$corona_result[question_2$corona_result == "positive"] <- 1
question_2$corona_result[question_2$corona_result != 1] <- 0

question_2 <- question_2[!question_2$age_60_and_above == "null" | !question_2$gender == "null",]

question_2 %>% count(is.na(age_60_and_above))
question_2 %>% count(is.na(gender))


#question 1.c, create test and train set
set.seed(123)
indx <- sample(2, nrow(question_2), replace = T, prob = c(0.9, 0.1))

train <- question_2[indx == 1, ]
test <- question_2[indx == 2, ]


#question 2.a, create logistical model using training 
model <- glm(train$corona_result == 1 ~ ., data = train, family = "binomial")
summary(model)

#question 2.b, create logistical model using whole dataset 
model2 <- glm(question_2$corona_result == 1 ~ ., data = question_2, family = "binomial")
train.prob <- predict(model2, newdata = question_2, type = "response")

#create confusion matrix with training set
train.prediction <- ifelse(train.prob >= 0.5, 1, 0)
table(question_2$corona_result, train.prediction)
1342 / (149 + 1342) #TP = .9, threshold = .5

#question 2.c, create confusion matrix with testing data set
test.prob <- predict(model, newdata = test, type = "response")

test.prediction <- ifelse(test.prob >= .2, 1, 0)
table(test$corona_result, test.prediction)
116 / (116 + 22) # TP = .84, threshold = .5
138 / ( 138 + 76) # TP = .64, threshold = .4
114 / ( 114 + 21) # TP = .84, threshold = .7
139 / ( 138 + 76) # TP = .64,thresh .2
184 / (184 + 236) # TP = .43  thresh .1




#question 2.f, create ROC curve using testing data
install.packages(ROCR)
library(ROCR)
PredictROC.prediction = predict(model, newdata = test)
pred = prediction(PredictROC.prob, test$corona_result)
perf = performance(pred, "tpr", "fpr")
plot(perf)

#question 2.hi, measure coefficient and impact on covid_results
coefficients(model)
#shortness_of_breath, 4.05173264 
exp(4.05173264 ) # = 57.49699

#question 2.hii
coefficients(model)
#head_ache, 5.87590378 
exp(5.87590378  ) # = 356.3466

#question 2.hiii
coefficients(model)
#age_60_and_aboveYes, -0.50565237 
exp(0.50565237  ) # = 1.658067

#question 3.a, create decision tree model based on training data
tree_model <- rpart(train$corona_result ~ ., data =train)
rpart.plot(tree_model)


#question 3.c, confusion matrix for decision tree model. 
#measured accuracy, true positive, and false positive

tree_model.prediction = predict(tree_model, newdata = test, type = "class")
table.output = table(test$corona_result, tree_model.prediction)
tp = table.output[2,2]
fp = table.output[1,2]
fn = table.output[2,1]
tn = table.output[1,1]
(tp + tn) / (tp + tn + fp +fn)
tp / (tp + fn)
fp / (fp + tn)

#question 3.d, 5 fold cross validation to find optimal cp


numFolds = trainControl( method = "cv", number = 5)
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

result = train(corona_result ~ ., 
               data = question_2, 
               method="rpart", 
               trControl = numFolds,
               tuneGrid = cpGrid)

result
