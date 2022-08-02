library(caret)
library(corrplot)
library(recipes)
library(glmnet)
library(rsample)
set.seed(1898)

# get conv scores
score <- readRDS("styleScores.RDS") %>%
  rename(score = style)

# Splitting (separate by criteria due to stratified split)
trainIndex <- createDataPartition(score$score, p = .7,
                                  list = FALSE,
                                  times = 1)

data <- readRDS("features.RDS") %>%
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
saveRDS(test, "convTest.RDS")

# transformation training-set
pp <- preProcess(select(train, -score),
                     method = c("center", "scale", "YeoJohnson"))
pp
trainT <- predict(pp, newdata = select(train, -score))
trainT$score <- train$score
summary(trainT)



# INTERACTIONS

# main effects recipe
main_rec <- recipe(score ~., train) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# interaction terms
int_vars <- colnames(select(train, -score))

interactions <- t(combn(as.character(int_vars), 2)) %>%
  as_tibble() %>%
  mutate(term = paste0(V1, ":", V2)) %>%
  filter(row_number() %in% 1500:1711) %>%
  pull(term) %>%
  paste(collapse = "+")






interactions <- paste(
  "TTR:nWordN",
  "PUNCT:ADP",
  "nPOStags:mCharUnqWord",
  "PUNCT:nEmotions",
  sep = "+"
)





interactions <- paste("~", interactions)
interactions <- as.formula(interactions)

int_rec <- recipe(score ~., train) %>%
  step_interact(interactions) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

# train models
train_folds <- vfold_cv(train)
train_ind <- rsample2caret(train_folds)

ctrl <-
  trainControl(
    method = "cv",
    index = train_ind$index,
    indexOut = train_ind$indexOut
  )

glmn_grid <- expand.grid(alpha = seq(.2, 1, by = .2), lambda = 10^seq(-4, -1, by = 0.1))

main_glmn <-
  train(main_rec,
        data = train,
        method = "glmnet",
        tuneGrid = glmn_grid,
        trControl = ctrl
  )

int_glmn <-
  train(int_rec,
        data = train,
        method = "glmnet",
        tuneGrid = glmn_grid,
        trControl = ctrl
  )

main_glmn$bestTune
int_glmn$bestTune

tune_plot <-
  int_glmn %>%
  pluck("results") %>%
  mutate(alpha = factor(alpha)) %>%
  ggplot(aes(x = lambda, y = RMSE, col = alpha)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  ggtitle("(a)") +
  theme(
    legend.position = c(0.50, 0.75),
    legend.background = element_blank()
  ) +
  labs(
    colour = "Mixing Percentage",
    x = "Regularization Parameter",
    y = "RMSE (Cross-Validation)"
  )


lasso_int_coefs <-
  int_glmn %>%
  pluck("finalModel") %>%
  coef(s = int_glmn %>% pluck("bestTune") %>% pluck("lambda")) %>%
  as.matrix() %>%
  as.data.frame() %>%
  setNames("Coefficient") %>%
  rownames_to_column("Predictor") %>%
  dplyr::filter(grepl("_x_", Predictor)) %>%
  arrange(desc(abs(Coefficient))) %>%
  dplyr::filter(Predictor != "(Intercept)")























# Recursive Feature Elimination ################################################

control <- rfeControl(functions = lmFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

rfe1 <- rfe(x = select(trainT, -score),
            y = trainT$score,
            sizes = c(10:20, 30, 40, 50, 60, 70, 80, 90),
            rfeControl = control)

# Print the results
rfe1

# Print the selected features
predictors(rfe1)

# Print the results visually
ggplot(data = rfe1, metric = "RMSE") + theme_bw()

# variable importance
varimp_data <- data.frame(feature = row.names(varImp(rfe1))[1:17],
                          importance = varImp(rfe1)[1:17, 1])

ggplot(data = varimp_data,
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") +
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) +
  theme_bw() + theme(legend.position = "none")
























































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


testT$predScore <- predict(model, testT)
sum(testT$score)
sum(testT$predScore)







