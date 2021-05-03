library(bigrquery)
library(DBI)
library(dplyr)
library(dbplyr)


con = dbConnect(bigrquery::bigquery(), project = "wsu-nfl-team1",dataset="kaggle_raw")

sqlStr = "select * from `wsu-nfl-team1.kaggle_raw.final7`"

df = dbGetQuery(con,sqlStr)


df$QBId <- as.factor(df$QBId)
df$team <- as.factor(df$team)
df$gameId <- as.factor(df$gameId)
df$playId <- as.factor(df$playId)
df$playDirection <- as.factor(df$playDirection)
df$team <- as.factor(df$team)
df$passResult <- as.factor(df$passResult)
df[, c('RId','playId','gameId')] <- NULL
set.seed(1)
train = base::sample(1:nrow(df),0.8*nrow(df))
test = (1:nrow(df))[-train]

trainingset = df[train,]
testingset = df[test,]

summary(df)

#random forest using CLASSIFICATION AND REGRESSION TRESS (CART)

library(caret)
library(doParallel)
cores <- makeCluster(detectCores()-1)
registerDoParallel(cores = cores)


tr <- trainControl(method = "cv",number = 10,allowParallel=TRUE,classProbs=TRUE)

random_forest <- train(passResult~., data = trainingset,method='ranger',trControl=tr,num.tree=100, importance = "impurity")

#accuracy of the training set 
rf.checkProb = predict(random_forest,trainingset,type = "prob")
rf.check = predict(random_forest,trainingset)
confusionMatrix(rf.check, trainingset$passResult)


#accuracy of the testing set
rf.predictProb = predict(random_forest,testingset,type="prob")
rf.predict = predict(random_forest,testingset)
confusionMatrix(rf.predict, testingset$passResult)




