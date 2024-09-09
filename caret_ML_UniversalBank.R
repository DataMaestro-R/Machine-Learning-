# Loading the require libraries

library(tidyverse)
library(caret)
library(caretEnsemble)
library(caretForecast)
library(mlbadata)


# Importing the dataset into the R environment

bank_data <- mlbadata::UniversalBank


bank_data$Personal.Loan <- factor(bank_data$Personal.Loan,
                                  labels = c("Yes", "No")) |> 
  relevel(ref = "Yes")

# Drop Zip.Code and recode education

bank_data <- bank_data |> 
  dplyr:: select(!c(ZIP.Code, ID)) |> 
  mutate(Education = factor(Education, levels = c(1,2,3),
                            labels = c("UGrad", "Grad", "Adv/Prof")))



# Data Partition

set.seed(1234)
trIndex <- createDataPartition(bank_data$Personal.Loan,
                               p = 0.8,
                               list = FALSE)


train_bank <- bank_data[trIndex, ]
test_bank <- bank_data[-trIndex, ]  
  
# PreProcessing

train.vals <- train_bank |> 
  preProcess(method = c("center", "scale"))


train_trnf <- predict(train.vals, train_bank)
test_trnf <- predict(train.vals, test_bank)


# Model Training (Basic Logistic Regression)

set.seed(89)
formula = Personal.Loan ~ Age + Income + CreditCard

fit_ctrl <- trainControl(method = "repeatedcv",
                         number = 5,
                         repeats = 3, 
                         allowParallel = TRUE)


logistic_mod1 <- train(formula, 
                       data = train_trnf, 
                       method = "glm",
                       family = "binomial",
                       trControl = fit_ctrl
                       )


logistic_mod1 |> 
  summary()

set.seed(89)

fit_ctrl2 <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          allowParallel = TRUE,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)
 

logistic_mod2 <- train(formula, 
                       data = train_trnf,
                       method  = "glm",
                       family = "binomial",
                       trControl = fit_ctrl2,
                       metric = "ROC"
                       ) 
  
logistic_mod2 |> 
  summary()
  

# Using the model to predict the outcome on the test set and obtain a confusion Matrix

test_trnf_pred <- predict(logistic_mod2, test_trnf)
confusionMatrix(data = test_trnf_pred,
                reference = test_trnf$Personal.Loan)
 

logistic_model3 <- train(formula,
                         data = train_bank,
                         preProc = c("center", "scale"),
                         method = "glm",
                         family = "binomial",
                         trControl = fit_ctrl2,
                         metric = "ROC"
                         ) 

logistic_model3 |> 
  summary()

test_pred <- predict(logistic_model3, test_trnf)
confusionMatrix(data = test_pred,
                reference = test_trnf$Personal.Loan)


# Comparing the Output of the two model

table(test_pred == test_trnf_pred)


# Tuning the Model parameters

glmn_grid <- expand.grid(.alpha = seq(0, 1, 0.1),
                         .lambda = seq(0.01, .3, length = 20)
                         )

glmnet_mod <- train(Personal.Loan ~ .,
                    data = train_bank,
                    preProc = c("center", "scale"),
                    trControl = fit_ctrl2,
                    method = "glmnet",
                    tuneGrid = glmn_grid,
                    metric = "ROC"
                    )

glmnet_mod |> 
  summary()

predictors(glmnet_mod)

# Observing the relative importance of these variables

m <- varImp(glmnet_mod) |> 
  ggplot(aes(predictors(glmnet_mod), Importance, fill = predictors(glmnet_mod))) +
  theme_minimal() + theme(plot.caption = element_text(family = "serif",
    size = 15, face = "italic"), axis.title = element_text(family = "serif",
    size = 15, face = "bold"), plot.title = element_text(family = "serif",
    size = 20, face = "bold.italic", hjust = 0.5)) +labs(title = "Important Features of the model.",
    caption = "Source: Mlbadata:: Universal Bank Data") + theme(axis.text = element_text(family = "serif",
    size = 15, face = "bold.italic"))

glmnClass <- predict(glmnet_mod, test_bank)
confusionMatrix(data = glmnClass,
                reference = test_bank$Personal.Loan)

names(getModelInfo())

# Training and resampling multiple methods

# Control for training multiple methods

methodCtrl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5,
                           index = createFolds(train_bank$Personal.Loan, 5),
                           classProbs = TRUE,
                           sampling = "down",
                           summaryFunction = twoClassSummary
                           )


methodList = c("rf", "glmnet", "svmRadial", "logreg")

# Train the ensemble models

set.seed(252)

ensemble <- caretEnsemble::caretList(Personal.Loan ~ .,
                      data = train_bank, 
                      # metric = "ROC",
                      trControl = methodCtrl,
                      preProc = c("center", "scale"),
                      # methodList = methodList,
                      tuneList = list(
                        rf = caretEnsemble::caretModelSpec(method = "rf",
                                            tunelength = 10),
                        glmnet = caretEnsemble::caretModelSpec(method = "glmnet",
                                                tunelength = 10),
                        svm = caretEnsemble::caretModelSpec(method = "svmRadial",
                                             tunelength = 5),
                        logit = caretEnsemble::caretModelSpec(method = "logreg",
                                               tunelength = 10)))
 
