library(tidyverse)
library(caret)
library(caretEnsemble)
library(gtsummary)

theme_set(new = theme_light())

# Importing the Snorena datasets in the R environment

snorena <- choose.files() |> 
  read.csv()


# Exploring the datasets

snorena |> 
  str(); snorena |> 
  glimpse()

snorena |> 
  names(); snorena |> 
  dim()

## Visualization of missing values

VIM::aggr(snorena)

# Data manipulation of the snorena datasets

snorena <- snorena |> 
  mutate(
    across(.cols = c("sex", "snore", "tobacco"),
           .fns = factor)
  )


a <- snorena |> 
  ggplot(
    aes(x = sex, 
        y = age, 
        fill = sex)
  ) + geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  scale_fill_manual(values = c("blue", "orange"))


b <- snorena |> 
  ggplot(
    aes(x = sex, 
        y = weight, 
        fill = sex)
  ) + geom_violin() +
  geom_jitter(alpha = 0.3) +
  scale_fill_manual(values = c("green", "navy"))

c <-  snorena |> 
  ggplot(
    aes(x = sex, 
        y = size, 
        fill = sex)
  ) + geom_violin() +
  geom_jitter(alpha = 0.3) +
  scale_fill_manual(values = c("violet", "red"))

d <-  snorena |> 
  ggplot(
    aes(x = sex, 
        y = alcohol, 
        fill = sex)
  ) + geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  scale_fill_manual(values = c("darkgreen", "yellow"))

gridExtra::grid.arrange(a, b,
                        c, d)


snorena |> 
  count(sex) |> 
  ggplot(
    aes(sex, n, fill = sex)
  ) +
  geom_col(color = "black")


snorena |> 
  count(snore) |> 
  ggplot(
    aes(snore, n, fill = snore)
  ) +
  geom_col(color = "black")

snorena |> 
  count(snore, sex) |> 
  ggplot(aes(sex, n, fill = snore)) +
  geom_col(color = "black", position = "dodge")+
  scale_fill_manual(values = c("red", 
                               "green"))

# Describing the dataset

snorena |> 
  summary()
psych::describe(snorena, 
                interp = TRUE)


# Building our predictive model

# Imputting missing values

msnorena <- snorena |> 
  preProcess(method = "bagImpute")
snorena <- msnorena |> 
  predict(snorena)

set.seed(444)

indexed <- createDataPartition(snorena$snore,
                               p = 0.8,
                               list = FALSE)

snorena_train <- snorena[indexed, ]
snorena_test <- snorena[-indexed, ]


vtrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5)


log_reg_model <- train(snore ~ .,
                       data = snorena_train,
                       method = "glm",
                       family = "binomial",
                       preProcess = c("center", 
                                      "scale"),
                       trControl = vtrl)

log_reg_model

log_reg_model |>
  summary()


nb_model <- train(snore ~ .,
                  data = snorena_train,
                  method = "naive_bayes",
                  preProcess = c("center", 
                                 "scale"),
                  trControl = vtrl)


svm_model <- train(snore ~ .,
                  data = snorena_train,
                  method = "svmRadial",
                  preProcess = c("center", 
                                 "scale"),
                  trControl = vtrl)









































































































