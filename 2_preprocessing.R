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

# save preprocessed features
saveRDS(features, "featuresPP.RDS")
