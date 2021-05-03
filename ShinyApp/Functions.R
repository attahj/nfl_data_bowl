library(bigrquery)
library(DBI)
library(dplyr)
library(dbplyr)
library(caret)


super_model <- readRDS("./final_model.rds")
sqlStr = "select * from  `wsu-nfl-team1.kaggle_raw.avgCompPctTable`"
df = dbGetQuery(con,sqlStr)
df = na.omit(df)

compModel = function(gId, pId, qId){

tmp <- df %>% filter(gameId == gId, playId == pId)
tmp$QBId <- qId
tmp$QBId <- as.factor(tmp$QBId)
rf.predictProb = predict(super_model,tmp,type="prob")
rf.predictProb[,1]

}