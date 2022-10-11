library(caret)
library(corrplot)
library(recipes)
library(glmnet)
library(rsample)
library(caTools)
library(ROSE)
library(DMwR2)
library(purrr)
library(caTools)
library(fastAdaboost)
library(naivebayes)
library(sjmisc)
library(ranger)
library(bnclassify)
library(broom)
library(e1071)
library(kernlab)
set.seed(1898)

# get Features
trainT <- readRDS("train_scaled_transformed.RDS")
trainPPT <- readRDS("train_scaled_transformed_preprocessed.RDS")
trainPPTW <- readRDS("train_scaled_transformed_preprocessed_with_BinaryWords.RDS")
testT <- readRDS("test_scaled_transformed.RDS")
testPPT <- readRDS("test_scaled_transformed_preprocessed.RDS")
testPPTW <- readRDS("test_scaled_transformed_preprocessed_with_BinaryWords.RDS")

# get CONV scores and flag cases that have score > 2 or score < 2
score <- readRDS("convScores.RDS") %>%
  rename(score = conv) %>%
  mutate(
    score1 = as.factor(ifelse(score == 1, "Y", "N")),
    score3 = as.factor(ifelse(score == 3, "Y", "N")),
    score = paste0("score", score)
  )
trainScore <- filter(score, id %in% trainT$id)

# get selected feature names
borutaFeatures <- readRDS("borutaFeatures.RDS")
selFeat1 <- borutaFeatures[[4]]
selFeat3 <- borutaFeatures[[16]]
selFeatPP1 <- readRDS("sfConv1")
selFeatPP3 <- readRDS("sfConv3")
selFeatPPW1 <- borutaFeatures[[12]]
selFeatPPW3 <- borutaFeatures[[24]]

################################################################################
################################################################################

# chose models
logReg <- FALSE
glmNetMC <- FALSE
glmNetSW <- FALSE
naiveBayes <- FALSE
randomForest <- FALSE
randomForestMC <- FALSE
svm <- FALSE

# performance calculation base table
perf <- filter(score, id %in% testT$id)

if(logReg) {

  # Logistic Regression ########################################################

  # create feature sets
  trainFeatures1 <- trainPPT %>%
    select(all_of(selFeatPP1)[1:17])

  trainFeatures3 <- trainPPT %>%
    select(all_of(selFeatPP3)[1:17])

  testFeatures <- testPPT %>%
    select(id, starts_with("score"), everything())

  trControl <- trainControl(
    method = 'repeatedcv',
    number = 10,
    repeats =  10,
    # summaryFunction = twoClassSummary,
    # classProbs = TRUE,
    # verboseIter = TRUE,
    # search = 'random',
    # sampling = 'rose'
  )

  # train model for score = 3 identification
  modelLogReg3 <- train(
    x = trainFeatures3,
    y = trainScore$score3,
    method = 'glm',
    family = 'binomial',
    trControl = trControl
  )
  modelLogReg3

  # train model for score = 1 identification
  modelLogReg1 <- train(
    x = trainFeatures1,
    y = trainScore$score1,
    method = 'glm',
    family = 'binomial',
    trControl = trControl
  )
  modelLogReg1

  # calculate class probabilities and score predictions
  perfLogReg <- testFeatures %>%
    left_join(score, by = "id") %>%
    mutate(
      pProbLogReg3 = predict(modelLogReg3, testFeatures, type = "prob"),
      pScoreLogReg3 = predict(modelLogReg3, testFeatures),
      pProbLogReg1 = predict(modelLogReg1, testFeatures, type = "prob"),
      pScoreLogReg1 = predict(modelLogReg1, testFeatures),
      pScoreLogReg = case_when(
        pScoreLogReg1 == "Y" & pScoreLogReg3 == "N" ~ "score1",
        pScoreLogReg1 == "N" & pScoreLogReg3 == "Y" ~ "score3",
        pScoreLogReg1 == "Y" & pScoreLogReg3 == "Y" ~ "score2",
        TRUE ~ as.character("score2")),
      pClassLogReg = ifelse(score == pScoreLogReg, 1, 0)
    ) %>%
    select(starts_with(c("score", "pProb", "pScore", "pClass")))

  sum(perfLogReg$pClassLogReg)/nrow(perfLogReg)

  # save LogReg results and add to perf-df
  saveLogReg <- select(perfLogReg, -(starts_with("score")))
  saveRDS(saveLogReg, "CONV_LogReg.RDS")
  perf <- cbind(perf, saveLogReg)

}

if(glmNetMC) {

# multiclass glmnet ############################################################

  trainFeatures <- trainT %>%
    select(-starts_with("score"))

  testFeatures <- testT %>%
    select(id, starts_with("score"), everything())

  trControl <- trainControl(
    # method = 'repeatedcv',
    # number = 10,
    # repeats =  10,
    # summaryFunction = twoClassSummary,
    classProbs = TRUE,
    verboseIter = FALSE,
    # search = 'grid',
    # sampling = 'rose'
  )

  modelGlmNetMC <- train(
    x = select(trainFeatures, -id),
    y = trainScore$score,
    # tuneGrid = expand.grid(
    #   alpha = 0:1,
    #   lambda = seq(0.0001, 1, length = 20)
    # ),
    method = "glmnet",
    family = "multinomial",
    type.multinomial = "grouped",
    trControl = trControl
  )

  modelGlmNetMC
  plot(modelGlmNetMC)

  # calculate class probabilities and score predictions
  perfGlmNetMC <- testFeatures %>%
    left_join(score, by = "id") %>%
    mutate(
      pProbGlmNetMC = predict(modelGlmNetMC, testFeatures, type = "prob"),
      pScoreGlmNetMC = predict(modelGlmNetMC, testFeatures),
      pClassGlmNetMC = ifelse(score == pScoreGlmNetMC, 1, 0)
    ) %>%
    select(starts_with(c("score", "pProb", "pScore", "pClass")))

  sum(perfGlmNetMC$pClassGlmNetMC)/nrow(perfGlmNetMC)

  # save GlmNetMC results and add to perf-df
  saveGlmNetMC <- select(perfGlmNetMC, -(starts_with("score")))
  saveRDS(saveGlmNetMC, "CONV_GlmNetMC.RDS")
  perf <- cbind(perf, saveGlmNetMC)

}

if(glmNetSW) {

# stepwise glmNet ##############################################################

  trainFeatures <- trainT %>%
    select(-starts_with("score"))

  testFeatures <- testT %>%
    select(id, starts_with("score"), everything())

 # train model for score = 3 identification
 trControl <- trainControl(
    # method = 'repeatedcv',
    # number = 10,
    # repeats =  10,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    verboseIter = FALSE,
    search = 'grid',
    # sampling = 'rose'
  )

 modelGlmNetSW3 <- train(
    x = select(trainFeatures, -id),
    y = trainScore$score3,
    # tuneGrid = expand.grid(
    #   alpha = c(0, 0.25, 0.5, 0.75, 1),
    #   lambda = seq(0.0001, 1, length = 20)
    # ),
    method = "glmnet",
    metric = "ROC",
    family = "binomial",
    trControl = trControl
  )

  modelGlmNetSW3
  plot(modelGlmNetSW3)
  max(modelGlmNetSW3[["results"]][["ROC"]])
  confusionMatrix(modelGlmNetSW3)

# # train model for score = 1 identification
modelGlmNetSW1 <- train(
  x = select(trainFeatures, -id),
  y = trainScore$score1,
  # tuneGrid = expand.grid(
  #   alpha = 0:1,
  #   lambda = seq(0.0001, 1, length = 20)
  # ),
  method = "glmnet",
  metric = "ROC",
  family = "binomial",
  trControl = trControl
)

modelGlmNetSW1
plot(modelGlmNetSW1)
max(modelGlmNetSW1[["results"]][["ROC"]])
confusionMatrix(modelGlmNetSW1)

# calculate class probabilities and score predictions
perfGlmNetSW <- testFeatures %>%
  left_join(score, by = "id") %>%
  mutate(
    pProbGlmNetSW3 = predict(modelGlmNetSW3, testFeatures, type = "prob"),
    pScoreGlmNetSW3 = predict(modelGlmNetSW3, testFeatures),
    pProbGlmNetSW1 = predict(modelGlmNetSW1, testFeatures, type = "prob"),
    pScoreGlmNetSW1 = predict(modelGlmNetSW1, testFeatures),
    pScoreGlmNetSW = case_when(
      pScoreGlmNetSW1 == "Y" & pScoreGlmNetSW3 == "N" ~ "score1",
      pScoreGlmNetSW1 == "N" & pScoreGlmNetSW3 == "Y" ~ "score3",
      pScoreGlmNetSW1 == "Y" & pScoreGlmNetSW3 == "Y" ~ "score2",
      TRUE ~ as.character("score2")),
    pClassGlmNetSW = ifelse(score == pScoreGlmNetSW, 1, 0)
    ) %>%
  select(starts_with(c("score", "pProb", "pScore", "pClass")))

sum(perfGlmNetSW$pClassGlmNetSW)/nrow(perfGlmNetSW)

# save GlmNetSW results and add to perf-df
saveGlmNetSW <- select(perfGlmNetSW, -(starts_with("score")))
saveRDS(saveGlmNetSW, "CONV_GlmNetSW.RDS")
perf <- cbind(perf, saveGlmNetSW)
}

if(naiveBayes) {

# naive bayes ##################################################################

  # create feature sets
  trainFeatures1 <- trainPPT %>%
    select(all_of(selFeatPP1))

  trainFeatures3 <- trainPPT %>%
    select(all_of(selFeatPP3))

  testFeatures <- testPPT %>%
    select(id, starts_with("score"), everything())

trControl <- trainControl(
  method = 'repeatedcv',
  number = 10,
  repeats =  10,
  # summaryFunction = twoClassSummary,
  # classProbs = TRUE,
  # verboseIter = TRUE,
  # search = 'random',
  # sampling = 'rose'
)

# train model for score = 3 identification
modelNB3 <- train(
  x = trainFeatures3,
  y = trainScore$score3,
  method = "nb",
  trControl = trControl
)

modelNB3
plot(modelNB3)

# train model for score = 1 identification
modelNB1 <- train(
  x = trainFeatures1,
  y = trainScore$score1,
  method = "nb",
  trControl = trControl
)

modelNB1
plot(modelNB1)

# calculate class probabilities and score predictions
perfNB <- testFeatures %>%
  left_join(score, by = "id") %>%
  mutate(
    pProbNB3 = predict(modelNB3, testFeatures, type = "prob"),
    pScoreNB3 = predict(modelNB3, testFeatures),
    pProbNB1 = predict(modelNB1, testFeatures, type = "prob"),
    pScoreNB1 = predict(modelNB1, testFeatures),
    pScoreNB = case_when(
      pScoreNB1 == "Y" & pScoreNB3 == "N" ~ "score1",
      pScoreNB1 == "N" & pScoreNB3 == "Y" ~ "score3",
      pScoreNB1 == "Y" & pScoreNB3 == "Y" ~ "score2",
      TRUE ~ as.character("score2")),
    pClassNB = ifelse(score == pScoreNB, 1, 0)
  ) %>%
  select(starts_with(c("score", "pProb", "pScore", "pClass")))

sum(perfNB$pClassNB)/nrow(perfNB)

# save NB results and add to perf-df
saveNB <- select(perfNB, -(starts_with("score")))
saveRDS(saveNB, "CONV_NB.RDS")
perf <- cbind(perf, saveNB)

}

if(randomForest) {

  # Random Forest #######################################################################

  # create feature sets
  trainFeatures1 <- trainPPT %>%
    select(all_of(selFeatPP1))

  trainFeatures3 <- trainPPT %>%
    select(all_of(selFeatPP3))

  testFeatures <- testPPT %>%
    select(id, starts_with("score"), everything())

  tgrid <- expand.grid(
    .mtry = 2:4,
    .splitrule = "gini",
    .min.node.size = c(10, 20)
  )

  trControl <- trainControl(
    method = 'cv',
    number = 5,
    classProbs = TRUE,
    verboseIter = TRUE,
    # sampling = 'rose'
  )

  # train model for score = 3 identification
  modelRandFor3 <- train(
    x = trainFeatures3,
    y = trainScore$score3,
    method = "ranger",
    trControl = trControl,
    tuneGrid = tgrid,
    num.trees = 500,
    importance = "impurity"
  )

  modelRandFor3
  plot(modelRandFor3)
  varImp(modelRandFor3)

  # train model for score = 1 identification
  modelRandFor1 <- train(
    x = trainFeatures1,
    y = trainScore$score1,
    method = "ranger",
    trControl = trControl,
    tuneGrid = tgrid,
    num.trees = 500,
    importance = "impurity"
  )

  modelRandFor1
  plot(modelRandFor1)

  # calculate class probabilities and score predictions
  perfRandFor <- testFeatures %>%
    left_join(score, by = "id") %>%
    mutate(
      pProbRandFor3 = predict(modelRandFor3, testFeatures, type = "prob"),
      pScoreRandFor3 = predict(modelRandFor3, testFeatures),
      pProbRandFor1 = predict(modelRandFor1, testFeatures, type = "prob"),
      pScoreRandFor1 = predict(modelRandFor1, testFeatures),
      pScoreRandFor = case_when(
        pScoreRandFor1 == "Y" & pScoreRandFor3 == "N" ~ "score1",
        pScoreRandFor1 == "N" & pScoreRandFor3 == "Y" ~ "score3",
        pScoreRandFor1 == "Y" & pScoreRandFor3 == "Y" ~ "score2",
        TRUE ~ as.character("score2")),
      pClassRandFor = ifelse(score == pScoreRandFor, 1, 0)
    ) %>%
    select(starts_with(c("score", "pProb", "pScore", "pClass")))

  sum(perfRandFor$pClassRandFor)/nrow(perfRandFor)

  # save RandFor results and add to perf-df
  saveRandFor <- select(perfRandFor, -(starts_with("score")))
  saveRDS(saveRandFor, "CONV_RandFor.RDS")
  perf <- cbind(perf, saveRandFor)

}

if(randomForestMC) {

  # Multiclass Random Forest ###################################################

  # create feature sets
  selFeat <- unique(c(selFeatPP1, selFeatPP3))

  trainFeatures <- trainPPT %>%
    select(all_of(selFeat), -id)

  testFeatures <- testPPT %>%
    select(id, starts_with("score"), everything())

  tgrid <- expand.grid(
    .mtry = 2:4,
    .splitrule = "gini",
    .min.node.size = c(10, 20)
  )

  trControl <- trainControl(
    method = 'cv',
    number = 5,
    classProbs = TRUE,
    verboseIter = TRUE,
    # sampling = 'rose'
  )

  # train model for score = 3 identification
  modelRandForMC <- train(
    x = trainFeatures,
    y = trainScore$score,
    method = "ranger",
    trControl = trControl,
    tuneGrid = tgrid,
    num.trees = 500,
    importance = "impurity"
  )

  modelRandForMC
  plot(modelRandForMC)
  varImp(modelRandForMC)

  # calculate class probabilities and score predictions
  perfRandForMC <- testFeatures %>%
    left_join(score, by = "id") %>%
    mutate(
      pProbRandForMC = predict(modelRandForMC, testFeatures, type = "prob"),
      pScoreRandForMC = predict(modelRandForMC, testFeatures),
      pClassRandForMC = ifelse(score == pScoreRandForMC, 1, 0)
    ) %>%
    select(starts_with(c("score", "pProb", "pScore", "pClass")))

  sum(perfRandForMC$pClassRandForMC)/nrow(perfRandForMC)

  # save RandForMC results and add to perf-df
  saveRandForMC <- select(perfRandForMC, -(starts_with("score")))
  saveRDS(saveRandForMC, "CONV_RandForMC.RDS")
  perf <- cbind(perf, saveRandForMC)

}

if(svm) {

  # Support Vector Machines ####################################################

  # create feature sets
  trainFeatures1 <- trainPPTW %>%
    select(all_of(selFeatPPW1))

  trainFeatures3 <- trainPPTW %>%
    select(all_of(selFeatPPW3))

  testFeatures <- testPPTW %>%
    select(id, starts_with("score"), everything())

  trControl <- trainControl(
    method = 'repeatedcv',
    number = 5,
    repeats = 5,
    classProbs = TRUE,
    verboseIter = TRUE,
    # sampling = 'rose'
  )

  # train model for score = 3 identification
  modelSVM3 <- train(
    x = trainFeatures3,
    y = trainScore$score3,
    method = "svmLinearWeights",
    trControl = trControl
    )

  modelSVM3
  plot(modelSVM3)
  varImp(modelSVM3)

  # train model for score = 1 identification
  modelSVM1 <- train(
    x = trainFeatures1,
    y = trainScore$score1,
    method = "svmLinearWeights",
    trControl = trControl
  )

  modelSVM1
  varImp(modelSVM1)

  # calculate class probabilities and score predictions
  perfSVM <- testFeatures %>%
    left_join(score, by = "id") %>%
    mutate(
      pProbSVM3 = predict(modelSVM3, testFeatures, type = "prob"),
      pScoreSVM3 = predict(modelSVM3, testFeatures),
      pProbSVM1 = predict(modelSVM1, testFeatures, type = "prob"),
      pScoreSVM1 = predict(modelSVM1, testFeatures),
      pScoreSVM = case_when(
        pScoreSVM1 == "Y" & pScoreSVM3 == "N" ~ "score1",
        pScoreSVM1 == "N" & pScoreSVM3 == "Y" ~ "score3",
        pScoreSVM1 == "Y" & pScoreSVM3 == "Y" ~ "score2",
        TRUE ~ as.character("score2")),
      pClassSVM = ifelse(score == pScoreSVM, 1, 0)
    ) %>%
    select(starts_with(c("score", "pProb", "pScore", "pClass")))

  sum(perfSVM$pClassSVM)/nrow(perfSVM)

  # save SVM results and add to perf-df
  saveSVM <- select(perfSVM, -(starts_with("score")))
  saveRDS(saveSVM, "CONV_SVM.RDS")
  perf <- cbind(perf, saveSVM)

}



# summarise and integrate multiple models in classification ####################

if(!logReg) {
  perf <- cbind(perf, readRDS("CONV_LogReg.RDS"))
}

if(!glmNetMC) {
  perf <- cbind(perf, readRDS("CONV_GlmNetMC.RDS"))
}

if(!glmNetSW) {
  perf <- cbind(perf, readRDS("CONV_GlmNetSW.RDS"))
}

if(!naiveBayes) {
  perf <- cbind(perf, readRDS("CONV_NB.RDS"))
}

if(!randomForest) {
  perf <- cbind(perf, readRDS("CONV_RandFor.RDS"))
}

if(!randomForestMC) {
  perf <- cbind(perf, readRDS("CONV_RandForMC.RDS"))
}

majVote <- perf %>%
  select(id, contains("score"), -ends_with(c("1", "3"))) %>%
  mutate(across(everything(), ~as.numeric(str_remove(., "score")))) %>%
  mutate(
    meanPScore = round(rowMeans(select(., starts_with("pScore"))), 0),
    correct = ifelse(score == meanPScore, 1, 0))

sum(majVote$correct)/nrow(majVote)

majVote %>%
  select(id, convScore = score, convPredScore = meanPScore) %>%
  saveRDS("CONV_finalScore")










incorrect <- majVote %>%
  mutate(meanPScore = rowMeans(select(., starts_with("pScore")))) %>%
  filter(correct == 0 & meanPScore %% 1 != 0)












perfAll <- select(perf, score, starts_with(c("predScore", "class"))) %>%
  mutate(
    mPredScore = rowMeans(select(., starts_with("predScore"))),
    mPredScore = round(mPredScore, 0),
    classTotal = ifelse(score == mPredScore, 1, 0)
    )









sum(perfAll$classTotal)/nrow(perfAll)

accuracies <- data.frame(
  baseLine = sum(data$score == 2)/nrow(data),
  logReg = sum(perfAll$classLogReg)/nrow(perfAll),
  glmNet = sum(perfAll$classglmNet)/nrow(perfAll),
  NB = sum(perfAll$classNB)/nrow(perfAll),
  randFor = sum(perfAll$classRandFor)/nrow(perfAll),
  total = sum(perfAll$classTotal)/nrow(perfAll)
)




