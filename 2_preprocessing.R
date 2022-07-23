library(caret)
library(corrplot)


# get features and remove id
features <- readRDS("features.RDS") %>%
  select(-id)

# check for near zero-variance variables
nzv <- nearZeroVar(features, saveMetrics= TRUE)
if(sum(nzv$nzv) > 0) {
  warning("near zero variance features in data!")
  remove <- rownames(filter(nzv, nzv))
  features <- select(features, -all_of(remove))
  print(remove)
}

# identify highly correlated (> 0.9) features
M <- cor(features)
corrplot(M)

corFeatures <- findCorrelation(M, cutoff = 0.9)
corFeatures <- names(features)[corFeatures]

if(length(corFeatures) > 0) {
  warning("highly correlated features in data!")
  features <- select(features, -all_of(corFeatures))
  print(corFeatures)
}

colnames(features)

# transformation
ideaPP <- preProcess(features,
                     method = c("center", "scale", "YeoJohnson"))
ideaPP
ideaTrainT <- predict(ideaPP, newdata = features)
summary(ideaTrainT)

saveRDS("featuresPP.RDS")









# get idea scores
score <- readRDS("ideaScores.RDS")

# Splitting (separate by criteria due to stratified split)
trainIndex <- createDataPartition(score$idea, p = .7,
                                  list = FALSE,
                                  times = 1)

idea <- features %>%
  mutate(id = row_number()) %>%
  left_join(score, by = "id") %>%
  select(-id)

ideaTrain <- idea[trainIndex,]
ideaTest <- idea[-trainIndex,]






