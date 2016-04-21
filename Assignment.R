testurl <- read.csv("pml-testing.csv")
trainingurl<-read.csv("pml-training.csv")
library(caret)
set.seed(123456)
trainset <- createDataPartition(trainingurl$classe, p = 0.8, list = FALSE)
Training <- trainingurl[trainset, ]
Validation <- trainingurl[-trainset, ]
# exclude near zero variance features
nzvcol <- nearZeroVar(Training)
Training <- Training[, -nzvcol]

# exclude columns with m40% ore more missing values exclude descriptive
# columns like name etc
cntlength <- sapply(Training, function(x) {
  sum(!(is.na(x) | x == ""))
})
nullcol <- names(cntlength[cntlength < 0.6 * length(trainingurl$classe)])
descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                 "cvtd_timestamp", "new_window", "num_window")
excludecols <- c(descriptcol, nullcol)
Training <- Training[, !names(Training) %in% excludecols]
library(randomForest)
rfModel <- randomForest(classe ~ ., data = Training, importance = TRUE, ntrees = 10)
ptraining <- predict(rfModel, Training)
print(confusionMatrix(ptraining, Training$classe))
pvalidation <- predict(rfModel, Validation)
print(confusionMatrix(pvalidation, Validation$classe))
ptest <- predict(rfModel, testurl)