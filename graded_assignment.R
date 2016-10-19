library(caret)
library(randomForest)
library(missForest)

setwd("~/Dropbox/coursera/practical_machine_learning")
dataset <- read.csv("pml-training.csv")


#check if there are zero or near-zero variance variablesz
nzv <- nearZeroVar(dataset, saveMetrics = TRUE)
#no variable has zero variance, but 60 have near-zero:
nzv
#to make computation faster,
#we try to work without near-zero variance variables:
nzv <- nearZeroVar(dataset)
dataset <- dataset[, -nzv]

#we remove columns that should not have predicting power:
#row numbers and time stamps (it's not a time series)
drops <- c("X",
           "raw_timestamp_part_1",
           "raw_timestamp_part_2",
           "cvtd_timestamp")
dataset <- dataset[ , !names(dataset) %in% drops]

#the later used randomForest algorithm gives error for partially filled datasets:
#let's try to fit a random Forest model only using 'complete' variables
NAvars <- sapply(dataset, function(x) mean(is.na(x))) > 0.95
dataset <- dataset[NAvars==FALSE]

#split the training dataset into training and validation sets
set.seed(113)
inTrain = createDataPartition(y = dataset$classe, p = 0.7, list=FALSE)
training = dataset[inTrain,]
validation = dataset[-inTrain,]


set.seed(211)
ctrl <- trainControl(method = "cv",
                     verboseIter = FALSE)
mod_rf <- train(classe ~ .,
                method = "rf",
                data = training,
                trControl = ctrl,
                #centers and scales predictors
                preProc = c("center", "scale"))


randomForClasses <- predict(mod_rf, newdata = validation)
#the random forest approach on the smaller but complete number of variables
#performs very well:
confusionMatrix(randomForClasses, validation$classe)
     

#finally, let's find the classes of the provided testing set.
assignment_testing <- read.csv("pml-testing.csv")
randomForClasses_test <- predict(mod_rf, newdata = assignment_testing)
randomForClasses_test
