# Load packages -----------------------------------------------------------

library(caret)


# Load the data sets ------------------------------------------------------

training <- read.csv("./pml-training.csv")
testing <- read.csv("./pml-testing.csv")

# Explore data set ------------------------------------------------
# structure
str(training)

# training: head and tail
head(training); tail(training)

# Clean the data sets -----------------------------------------------------

# remove columns with undesired info
training <- training[, -c(1:7)]
testing <- testing[, -c(1:7)]

# remove columns with NA        
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

# remove all character columns except classe
training <- training[, sapply(training[ , -86], is.numeric)]
testing <- testing[, sapply(testing[ , -86], is.numeric)]

# dim
dim(training); dim(testing)

# Split training data -----------------------------------------------------
set.seed(1222)

inTrain <- createDataPartition(y = training$classe, 
                               p = 0.7,
                               list = FALSE)

trainingset <- training[inTrain, ]
testingset <- training[-inTrain, ]


# fit models --------------------------------------------------------------

# classification tree
modrpart <- train(classe ~., method = "rpart",  data = trainingset)

plot(modrpart$finalModel, main = "Classification tree")
text(modrpart$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)

modrpart$
# predict using testingset
predrpart <- predict(modrpart, testingset)

# evaluate the model
confusionMatrix(predrpart, as.factor(testingset$classe))

# gbm
control <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
modgbm <- train(classe ~., method = "gbm",  data = trainingset, 
                trControl = control, verbose = FALSE)

# predict using testingset
predgbm <- predict(modgbm, testingset)

# evaluate
confusionMatrix(predgbm, as.factor(testingset$classe))

# rf
modrf <- train(classe ~., method = "rf", data = trainingset,
               trControl = trainControl(method = "cv"), number = 3)

# predict using testingset
predrf <- predict(modrf, testingset)

sqrt(mean((prediction - as.numeric(testingset))^2))

# evaluate
confusionMatrix(predrf, as.factor(testingset$classe))


# Apply selected model ----------------------------------------------------

predfinal <- predict(modrf, testing)
predfinal
