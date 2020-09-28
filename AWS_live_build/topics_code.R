## Author: Rohnin Randles, University of Washington-Seattle
## Date: July 31, 2020
##
## Purpose: The purpose of this code is to create
## a supervised learning model for topic modeling
## of Washington state legislation based on the
## text of the bill title and description

####### Random Forest Code

library(randomForest)
library(caTools)
library(rvest)
library(tm)
library(SnowballC)

topic_base <- read.csv("Source Data/topic_baseData.csv", header = TRUE, stringsAsFactors = FALSE)

mlData <- topic_base %>% select(LegalTitle, goodlabels)

corpus <- VCorpus(VectorSource(mlData$LegalTitle))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)

saveRDS(dtm, "Source Data/trainedDTM.rds")

data <- as.data.frame(as.matrix(dtm))
data$topicLabel <- factor(mlData$goodlabels)

sample <- sample.split(data$topicLabel, SplitRatio = 0.75)

training <- subset(data, sample == TRUE)
test <- subset(data, sample == FALSE)

rf <- randomForest(x = training[,-1091], y = training$topicLabel, ntree = 100, do.trace = 10)

pred <- predict(rf, test[,-1091])
cm <- table(test[,1091], pred)
correct <- 0
total <- 0
for(i in 1:nrow(cm)){
  print(paste(cm[i,i], "/", sum(cm[,i]), " = ", round(100*cm[i,i]/sum(cm[,i]), 2), "%", sep = ""))
  correct <- correct + cm[i,i]
  total <- total + sum(cm[,i])
}
print(paste("Aggregate: ", correct, "/", total, " = ", round(100*correct/total, 2), "%", sep = ""))

saveRDS(rf, "Source Data/rfTopicLabels.rds")
