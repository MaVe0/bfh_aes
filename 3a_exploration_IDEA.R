library(caret)
library(corrplot)
set.seed(1898)

# get idea scores
score <- readRDS("ideaScores.RDS") %>%
  rename(score = idea)

# Splitting (separate by criteria due to stratified split)
trainIndex <- createDataPartition(score$score, p = .7,
                                  list = FALSE,
                                  times = 1)

data <- readRDS("featuresPP.RDS") %>%
  mutate(id = row_number()) %>%
  left_join(score, by = "id") %>%
  select(-id) %>%
  mutate(score = as.numeric(score))

train <- data[trainIndex,]
test <- data[-trainIndex,]

# transformation test-set
pp <- preProcess(select(test, -score),
                 method = c("center", "scale", "YeoJohnson"))
pp
testT <- predict(pp, newdata = select(test, -score))
testT$score <- test$score
summary(testT)


saveRDS(test, "ideaTest.RDS")

# transformation training-set
pp <- preProcess(select(train, -score),
                     method = c("center", "scale", "YeoJohnson"))
pp
trainT <- predict(pp, newdata = select(train, -score))
trainT$score <- train$score
summary(trainT)

# Perform Boruta search
library(Boruta)
boruta_output <- Boruta(score ~ ., trainT, doTrace=2)
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")


# Fit lm model using 5 x 5-fold CV: model
model <- train(
  score ~ .,
  trainT,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10,
    verboseIter = TRUE
  )
)

varImp(model)



# Exploration

# load the library
library(mlbench)

set.seed(10)

subsets <- c(1:5, 10, 15, 20, 25)

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

x <- select(trainT, -score)
y <- trainT$score

lmProfile <- rfe(x,
                 y,
                 sizes = subsets,
                 rfeControl = ctrl)

predictors(lmProfile)

trainT <- select(trainT, all_of(predictors(lmProfile)))
trainT$score <- train$score














library(gbm)
model2 <- train(score ~ .,
                trainT,
                method = "gbm",
                # trControl = fitControl,
                # ## This last option is actually one
                # ## for gbm() that passes through
                verbose = FALSE
                )


# Print model to console
model2


testT$predScore <- predict(model2, testT)
sum(testT$score)
sum(testT$predScore)







