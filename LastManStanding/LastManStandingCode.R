# Last man standing

library(h2o)
library(mice)

path <- 'C:\\Users\\mbarnwa\\Desktop\\LastManStanding\\data'
setwd(path)
dir()

trainLms <- read.csv("Train_Fyxd0t8.csv")
testLms <- read.csv("Test_C1XBIYq.csv")

dim(trainLms)
dim(testLms)

str(trainLms)
testLms$Crop_Damage <- 99

testLmsId <- testLms$ID
trainLms$ID = testLms$ID = NULL

# Number_weeks_used only has missing values in train data -9000
apply(trainLms, 2, function(x) sum(is.na(x)))

# combining train and test data together
combiLms <- rbind(trainLms, testLms)
dim(combiLms)

catgCols <- c('Crop_Type', 'Soil_Type', 'Pesticide_Use_Category', 'Season', 'Crop_Damage')
discreteCols <- c('Estimated_Insects_Count', 'Number_Doses_Week', 'Number_Weeks_Used', 'Number_Weeks_Quit')

combiLms[catgCols] <- lapply(combiLms[catgCols], as.factor)
combiLms[discreteCols] <- lapply(combiLms[discreteCols], as.numeric)
str(combiLms)

# impute missing value using 'mice' package
combiLms[,-9] <-complete(mice(combiLms[,-9], method = c("", "", "", "", "", "pmm", "", "")))

# saving the imputed dataframe here
combiLmsSaved <- combiLms

# Separating 'combi' to test and train
testInd <- combiLms$Crop_Damage==99
head(testInd)

dim(combiLms)
testData <- combiLms[testInd,]
# write.csv(testData, 'testDataImputed.csv', row.names = F)
dim(testData)

trainData <- combiLms[!testInd,]
# write.csv(trainData, 'trainDataImputed.csv', row.names = F)
dim(trainData)
str(combiLms)

# Feature engg

# trainData
numberTrainDataRows <- nrow(trainData)

# Crop_Type
CTCountTr <- table(trainData$Crop_Type)/numberTrainDataRows
trainData$CropTypeCount <- ifelse(trainData$Crop_Type==0, CTCountTr[1], CTCountTr[2])
# combiLmsTry$CropTypeCount <- NULL
head(trainData)

table(combiLms$Crop_Type, combiLms$Crop_Damage)

# Soil_Type
STCountTr <- table(trainData$Soil_Type)/numberTrainDataRows
trainData$SoilTypeCount <- ifelse(trainData$Soil_Type==0, STCountTr[1], STCountTr[2])
head(trainData)

# Pesticide_Use_Category
PUCCountTr <- table(trainData$Pesticide_Use_Category)/numberTrainDataRows
trainData$PesticideUseCount <- ifelse(trainData$Pesticide_Use_Category==2, PUCCountTr[2], PUCCountTr[3])
trainData$PesticideUseCount[which(trainData$Pesticide_Use_Category==1)] <- PUCCountTr[1]

# Season
SeasonCntTr <- table(trainData$Season)/numberTrainDataRows
trainData$SeasonCount <- ifelse(trainData$Season ==2, SeasonCntTr[2], SeasonCntTr[3])
trainData$SeasonCount[which(trainData$Season==1)] <- SeasonCntTr[1]
str(trainData)

# -----
# testData
numberTestDataRows <- nrow(testData)

# Crop_Type
CTCountTe <- table(testData$Crop_Type)/numberTestDataRows
testData$CropTypeCount <- ifelse(testData$Crop_Type==0, CTCountTe[1], CTCountTe[2])
# combiLmsTry$CropTypeCount <- NULL
head(testData)


# Soil_Type
STCountTe <- table(testData$Soil_Type)/numberTestDataRows
testData$SoilTypeCount <- ifelse(testData$Soil_Type==0, STCountTe[1], STCountTe[2])
head(testData)

# Pesticide_Use_Category
PUCCountTe <- table(testData$Pesticide_Use_Category)/numberTestDataRows
testData$PesticideUseCount <- ifelse(testData$Pesticide_Use_Category==2, PUCCountTe[2], PUCCountTe[3])
testData$PesticideUseCount[which(testData$Pesticide_Use_Category==1)] <- PUCCountTe[1]

# Season
SeasonCntTe <- table(testData$Season)/numberTestDataRows
testData$SeasonCount <- ifelse(testData$Season ==2, SeasonCntTe[2], SeasonCntTe[3])
testData$SeasonCount[which(testData$Season==1)] <- SeasonCntTe[1]
str(testData)

dim(trainData)
dim(testData)


write.csv(trainData, 'trainDataImputedFE.csv', row.names = F)
testDataCopy <- testData
testDataCopy$Crop_Damage <- NULL
write.csv(testDataCopy, 'testDataImputedFE.csv', row.names = F)

# Ran gbm in h2o UI
dl_model_3 = h2o.gbm( x=features,
                      y = "Crop_Damage",
                      training_frame =train.hex ,
                      #validation_frame =testHex ,
                      max_depth = 3,
                      distribution = "gaussian",
                      ntrees =700,
                      learn_rate = 0.01,
                      nbins_cats = 1024
)


