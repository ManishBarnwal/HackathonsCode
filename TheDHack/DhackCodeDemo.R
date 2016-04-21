# Author: Manish Barnwal
# Email: manishbarnwal.iit@gmail.com
# Project: The D hack- a classification problem

library(h2o)
# localH2O = new("H2OClient", ip = "127.0.0.1", port = 54321)

localH2O=h2o.init()
# localH2O=h2o.init(nthreads = -1, max_mem_size = '5g')

setwd("C:/Users/mbarnwa/Desktop/D Hack/data")
dir()
# train <- read.csv("frame_0.750.csv")
train <- read.csv("train_FBFog7d.csv")
colnames(train)

factorCols <- c("Var1", "WorkStatus", "Divorce", "Widowed", "Residence_Region", "income",
                "Engagement_Religion", "Var2", "Gender", "Unemployed10", "Alcohol_Consumption")

# Converting character columns to factor
train[factorCols] <- lapply(train[factorCols], as.factor)
apply(train, 2, function(x) sum(is.na(x)))

apply(train, 2, function(x) length(unique(x)))

# plot(train$Score)
numericCols <- c('Score', 'Education', 'babies', 'preteen', 'teens', 'TVhours')
apply(train[,numericCols], 2, function(x) sum(is.na(x)))

summary(train$Score)
unique(train$Score)
data.frame(table(train$Score))
train$Score[is.na(train$Score)] <- median(train$Score, na.rm = T)

# plot(train$Education)
summary(train$Education)
unique(train$Education)
data.frame(table(train$Education))
train$Education[is.na(train$Education)] <- median(train$Education, na.rm = T)

# train[is.na(train)] <- 999

# Lare number of missing values so we will impute it by a random value
train$TVhours[is.na(train$TVhours)] <- 999

# We will drop the remaining missing value columns or not use them in the model
# For now let's replace by median
train$babies[is.na(train$babies)] <- median(train$babies, na.rm = T)
train$preteen[is.na(train$preteen)] <- median(train$preteen, na.rm = T)
train$teens[is.na(train$teens)] <- median(train$teens, na.rm = T)


# function to get mode(max frequency) of a column
getMode <- function(x) {
        tmp <- table(x)
        max <- which.max(tmp)
        names(max)        
}


train_temp <- train
apply(train_temp[, factorCols], 2, function(x) sum(is.na(x)))

# getMode(train_temp$Divorce)
# sum(is.na(train_temp$Divorce))
# train_temp[is.na(train_temp[,'Divorce']), 'Divorce'] <- getMode(train_temp[,'Divorce'])

for(i in factorCols) {
        print(i)
        train_temp[is.na(train_temp[,i]), i] <- getMode(train_temp[,i])
        # print(getMode(train_temp[,i]))
}

train <- train_temp

# Before modelling by h2o on any dataset, the dataset has to be converted into hex form 
trainHex <- as.h2o(train)

# Splitting the training data for validation
trainSplit <- h2o.splitFrame(trainHex, ratios = 0.7)

trainDataHex <- trainSplit[[1]]
valiDataHex <- trainSplit[[2]]
summary(trainDataHex)

# test data
test <- read.csv("Test_L4P23N3.csv")
colnames(test)
test[factorCols] <- lapply(test[factorCols], as.factor)

# Converting to h2o format
testDataHex<-as.h2o(test)

colnames(testDataHex)

# not including the response column, 'Happy' and the 'ID' column
features<-colnames(train)[!(colnames(train) %in% c("Happy", "ID"))] 
# features1 <- colnames(train)[!(colnames(train) %in% c("Happy", "ID", "Widowed", 
#                                                      "babies", "teens","Unemployed10", "Divorce", "Gender"))]
# length(features1)
# as.integer(sqrt(length(features1)))
# mtries = c(2, as.integer(sqrt(length(features1))), 4, 5, 6)
# mtries=c(2, 3, 4,5)

# ntrees =  c(100, 200, 350, 500)
# ntrees = c(50, 80, 100, 120, 150)

# df <- data.frame()
# zz <- NA

rfHex <- h2o.randomForest(x=features,
                          y="Happy", 
                          training_frame=trainDataHex,
                          # validation_frame = testHex,
                          ntrees = 200,
                          mtries = -1,
                          max_depth = 20,
                          balance_classes = TRUE,
                          sample_rate = 0.65
                          )
                
# rfHex
summary(rfHex)
                
ans<-as.data.frame(h2o.predict(rfHex,testDataHex))
head(ans)
predicted <- ans$predict
head(predicted)
# str(predicted)
# levels(predicted) <- c('5', '10', '15')
# predicted <- as.numeric(as.character(predicted))
# head(predicted)


h2o.ls() # gives you the list of objects created in the session
h2o.clusterInfo() # gives information about the cluster
h2o.shutdown() # shuts down the cluster


