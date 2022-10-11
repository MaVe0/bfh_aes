library(caret)
library(corrplot)
library(tidylog)

# Preprocess train set #########################################################

# get features and remove id
features <- readRDS("trainFeatures.RDS")
trainIds <- features$id
features <- select(features, -id)

# no categorial predictors

# remove features based on absolute counts except those directly related to text length
nFeatures <- select(features, starts_with("n"), -starts_with(c("nWord", "nChar", "nWS"))) %>%
  colnames()
featuresPP <- select(features, -all_of(nFeatures))
colnames(featuresPP)

# identify near zero-variance variables
nzv <- nearZeroVar(featuresPP, saveMetrics= TRUE)
zvFeatures <- rownames(filter(nzv, nzv))
featuresPP <- select(featuresPP, -all_of(zvFeatures))

# identify highly correlated (> 0.8) features
M <- cor(featuresPP)
# corrPlot <- corrplot(M, tl.pos='n')
# saveRDS(corrplot(M, tl.pos='n'), "corrplot")
corFeatures <- findCorrelation(M, cutoff = 0.8)
corFeatures <- names(featuresPP)[corFeatures]
featuresPP <- select(featuresPP, -all_of(corFeatures))

# identify linear dependencies
linDep <- findLinearCombos(featuresPP)
# none

# transformation training-set on unprocessed features
pp <- preProcess(features, method = c("center", "scale", "YeoJohnson"))
trainT <- predict(pp, newdata = features)
trainT$id <- trainIds
traintT <- select(trainT, id, everything())
saveRDS(trainT, "train_scaled_transformed.RDS")

# transformation training-set on processed features
pp <- preProcess(featuresPP, method = c("center", "scale", "YeoJohnson"))
trainPPT <- predict(pp, newdata = featuresPP)
trainPPT$id <- trainIds
traintPPT <- select(trainPPT, id, everything())
saveRDS(trainPPT, "train_scaled_transformed_preprocessed.RDS")

# Preprocess test set #########################################################

# get features and remove id
features <- readRDS("testFeatures.RDS")
testIds <- features$id
features <- select(features, -id)

featuresPP <- features %>%
  select(-all_of(zvFeatures)) %>%
  select(-all_of(nFeatures)) %>%
  select(-all_of(corFeatures))

# transformation testing-set on unprocessed features
pp <- preProcess(features, method = c("center", "scale", "YeoJohnson"))
testT <- predict(pp, newdata = features)
testT$id <- testIds
testT <- select(testT, id, everything())
saveRDS(testT, "test_scaled_transformed.RDS")

# transformation testing-set on processed features
pp <- preProcess(featuresPP, method = c("center", "scale", "YeoJohnson"))
testPPT <- predict(pp, newdata = featuresPP)
testPPT$id <- testIds
testPPT <- select(testPPT, id, everything())
saveRDS(testPPT, "test_scaled_transformed_preprocessed.RDS")

# Preprocess binary word matrix ################################################

# TRAIN
trainBinary <- readRDS("trainBinaryWords.RDS")
trainIds <- trainBinary$id
trainBinary <- select(trainBinary, -id)

# identify near zero-variance variables
nzv <- nearZeroVar(trainBinary, saveMetrics= TRUE)
zvFeatures <- rownames(filter(nzv, nzv))
trainBinary <- select(trainBinary, -all_of(zvFeatures))

# TEST
testBinary <- readRDS("testBinaryWords.RDS")
testIds <- testBinary$id
testBinary <- select(testBinary, -id)

# identify near zero-variance variables
nzv <- nearZeroVar(testBinary, saveMetrics= TRUE)
zvFeatures <- rownames(filter(nzv, nzv))
testBinary <- select(testBinary, -all_of(zvFeatures))

# keep features that vary in both
keep <- Reduce(intersect, list(colnames(testBinary), colnames(trainBinary)))
trainBinary <- select(trainBinary, all_of(keep))
testBinary <- select(testBinary, all_of(keep))

# add binary words to features
trainPPTW <- cbind(trainPPT, trainBinary)
saveRDS(trainPPTW, "train_scaled_transformed_preprocessed_with_binaryWords.RDS")

testPPTW <- cbind(testPPT, testBinary)
saveRDS(testPPTW, "test_scaled_transformed_preprocessed_with_binaryWords.RDS")
















# get features and remove id
features <- readRDS("binaryWords.RDS")
testIds <- features$id
features <- select(features, -id)

# identify near zero-variance variables
nzv <- nearZeroVar(features, saveMetrics= TRUE)
zvFeatures <- rownames(filter(nzv, nzv))
featuresPP <- select(features, -all_of(zvFeatures))

# identify highly correlated (> 0.9) features
M <- cor(featuresPP)
corFeatures <- findCorrelation(M, cutoff = 0.85)
corFeatures <- names(featuresPP)[corFeatures]
featuresPP <- select(featuresPP, -all_of(corFeatures))

# identify linear dependencies
linDep <- findLinearCombos(featuresPP)
# none









# transformation testing-set on unprocessed features
pp <- preProcess(features, method = c("center", "scale", "YeoJohnson"))
testT <- predict(pp, newdata = features)
testT$id <- testIds
testT <- select(testT, id, everything())
saveRDS(testT, "test_scaled_transformed.RDS")

# transformation testing-set on processed features
pp <- preProcess(featuresPP, method = c("center", "scale", "YeoJohnson"))
testPPT <- predict(pp, newdata = featuresPP)
testPPT$id <- testIds
testPPT <- select(testPPT, id, everything())
saveRDS(testPPT, "test_scaled_transformed_preprocessed.RDS")
