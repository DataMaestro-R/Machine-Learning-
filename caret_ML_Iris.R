library(tidyverse)
library(caret)

theme_set(new = theme_light())

# Building a predictive model using the caret pacakge using rf, knn, lda and svm
# evaluating which would have the higest accuracy in predicting the Iris species.


# Importing the Iris data into the R enivronment and Exploring the dataset

iris |>
  glimpse()

names(iris)

is.na(iris)


# Descriptive analysis of the Iris data

psych::describe(iris[, c(1:4)]) |>
  dplyr::select(mean, sd, median,
                min, max, range,
                skew, se)


iris$Species |>
  summary()

iris |>
  dplyr:: select(!Species) |>
  cor() |>
  corrplot::corrplot(
    method = "pie",
    type = "upper",
    diag = FALSE,
    title = "A Correlation Plot of the components of the Iris data."
  )

# Visualization of the Iris datasets
theme_set(new = theme_light())
iris |>
  gather(key = Attribute, value = Measurement,
         Sepal.Length, Sepal.Width) |>
  dplyr::select(Species, Attribute, Measurement) |>
  ggplot(aes(Attribute, Measurement, fill = Species)) +
  geom_boxplot() +
  facet_wrap(~ Species, scales = "free") +
  scale_fill_manual(values = c("red",
                               "navy",
                               "darkgreen")) + theme(
                                 plot.subtitle = element_text(
                                   family = "serif",
                                   size = 12,
                                   face = "italic"
                                 ),
                                 plot.caption = element_text(
                                   family = "serif",
                                   size = 15,
                                   face = "italic"
                                 ),
                                 axis.ticks = element_line(linetype = "blank"),
                                 axis.title = element_text(
                                   family = "serif",
                                   size = 15,
                                   face = "bold.italic"
                                 ),
                                 axis.text = element_text(
                                   family = "serif",
                                   size = 15,
                                   face = "bold.italic"
                                 ),
                                 plot.title = element_text(
                                   family = "serif",
                                   size = 20,
                                   face = "bold.italic"
                                 ),
                                 legend.text = element_text(
                                   size = 12,
                                   face = "italic",
                                   family = "serif"
                                 ),
                                 legend.title = element_text(
                                   size = 15,
                                   face = "bold",
                                   family = "serif"
                                 ),
                                 legend.position = "top",
                                 legend.direction = "horizontal"
                               ) +
  labs(title = "A boxplot of the Attribute of the Iris data.",
       subtitle = "(Sepal.Length and Sepal.Width)",
       caption = "Source:: Iris dataset") +
  theme(panel.grid.minor = element_line(linetype = "blank")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "grey95")) + theme(plot.subtitle = element_text(hjust = 0.5))



iris |>
  gather(key = Attribute, value = Measurement,
         Petal.Length, Petal.Width) |>
  dplyr:: select(Species, Attribute, Measurement) |>
  ggplot(aes(Attribute, Measurement, fill = Species)) +
  geom_boxplot() +
  facet_wrap( ~ Species, scales = "free_y") +
  scale_fill_manual(values = c("purple",
                               "orange",
                               "green")) + theme(
                                 plot.subtitle = element_text(
                                   family = "serif",
                                   size = 12,
                                   face = "italic"
                                 ),
                                 plot.caption = element_text(
                                   family = "serif",
                                   size = 15,
                                   face = "italic"
                                 ),
                                 axis.ticks = element_line(linetype = "blank"),
                                 axis.title = element_text(
                                   family = "serif",
                                   size = 15,
                                   face = "bold.italic"
                                 ),
                                 axis.text = element_text(
                                   family = "serif",
                                   size = 15,
                                   face = "bold.italic"
                                 ),
                                 plot.title = element_text(
                                   family = "serif",
                                   size = 20,
                                   face = "bold.italic"
                                 ),
                                 legend.text = element_text(
                                   size = 12,
                                   face = "italic",
                                   family = "serif"
                                 ),
                                 legend.title = element_text(
                                   size = 15,
                                   face = "bold",
                                   family = "serif"
                                 ),
                                 legend.position = "top",
                                 legend.direction = "horizontal"
                               ) +
  labs(title = "A boxplot of the Attribute of the Iris data.",
       subtitle = "(Petal.Length and Petal.Width)",
       caption = "Source:: Iris dataset") +
  theme(panel.grid.minor = element_line(linetype = "blank")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "grey95")) +
  theme(plot.subtitle = element_text(hjust = 0.5))+geom_jitter()


iris |>
  ggplot(aes(Sepal.Length, Sepal.Width, colour = Species)) +
  geom_point(size = 3, alpha = 1) + facet_wrap(~ Species, scales = "free") +
  scale_colour_manual(values = c("red",
                                 "navy",
                                 "darkgreen")) + theme(
                                   plot.subtitle = element_text(
                                     family = "serif",
                                     size = 12,
                                     face = "italic"
                                   ),
                                   plot.caption = element_text(
                                     family = "serif",
                                     size = 15,
                                     face = "italic"
                                   ),
                                   axis.ticks = element_line(linetype = "blank"),
                                   axis.title = element_text(
                                     family = "serif",
                                     size = 15,
                                     face = "bold.italic"
                                   ),
                                   axis.text = element_text(
                                     family = "serif",
                                     size = 15,
                                     face = "bold.italic"
                                   ),
                                   plot.title = element_text(
                                     family = "serif",
                                     size = 20,
                                     face = "bold.italic"
                                   ),
                                   legend.text = element_text(
                                     size = 12,
                                     face = "italic",
                                     family = "serif"
                                   ),
                                   legend.title = element_text(
                                     size = 15,
                                     face = "bold",
                                     family = "serif"
                                   ),
                                   legend.position = "top",
                                   legend.direction = "horizontal"
                                 ) +
  labs(title = "A Scatter Plot of the Iris data.",
       subtitle = "(Sepal.Length and Sepal.Width)",
       caption = "Source:: Iris dataset") +
  theme(panel.grid.minor = element_line(linetype = "blank")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "grey95")) + theme(plot.subtitle = element_text(hjust = 0.5))





iris |>
  ggplot(aes(Petal.Length, Petal.Width, colour = Species)) +
  geom_point(size = 3, alpha = 1) + facet_wrap( ~ Species) +
  scale_colour_manual(values = c("purple",
                                 "orange",
                                 "green")) + theme(
                                   plot.subtitle = element_text(
                                     family = "serif",
                                     size = 12,
                                     face = "italic"
                                   ),
                                   plot.caption = element_text(
                                     family = "serif",
                                     size = 15,
                                     face = "italic"
                                   ),
                                   axis.ticks = element_line(linetype = "blank"),
                                   axis.title = element_text(
                                     family = "serif",
                                     size = 15,
                                     face = "bold.italic"
                                   ),
                                   axis.text = element_text(
                                     family = "serif",
                                     size = 15,
                                     face = "bold.italic"
                                   ),
                                   plot.title = element_text(
                                     family = "serif",
                                     size = 20,
                                     face = "bold.italic"
                                   ),
                                   legend.text = element_text(
                                     size = 12,
                                     face = "italic",
                                     family = "serif"
                                   ),
                                   legend.title = element_text(
                                     size = 15,
                                     face = "bold",
                                     family = "serif"
                                   ),
                                   legend.position = "top",
                                   legend.direction = "horizontal"
                                 ) +
  labs(title = "A Scatter Plot of the Iris data.",
       subtitle = "(Petal.Length and Petal.Width)",
       caption = "Source:: Iris dataset") +
  theme(panel.grid.minor = element_line(linetype = "blank")) + theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = "grey95")) +
  theme(plot.subtitle = element_text(hjust = 0.5))


# Model training using the Caret package

set.seed(2024)
cIndex <- createDataPartition(iris$Species,
                              p = 0.7,
                              list = FALSE)


train_iris <- iris[cIndex, ]
test_iris <- iris[-cIndex, ]


ctrl <- trainControl(method = "repeatedcv",
                     number = 10, 
                     repeats = 3,
                     allowParallel = T
                     )



rf_model <- train(Species ~ ., 
                  data = train_iris,
                  method = "rf",
                  preProc = c("center", "scale"),
                  trControl = ctrl
                  )


knn_model <- train(Species ~ .,
                   data = train_iris,
                   method = "knn",
                   preProc = c("center", "scale"),
                   trControl = ctrl
                   )


lda_model <- train(Species ~ .,
                   data = train_iris,
                   method = "lda",
                   preProc = c("center", "scale"),
                   trControl = ctrl
                   )


svm_model <- train(Species ~ .,
                   data = train_iris,
                   method = "svmRadial",
                   preProc = c("center", "scale"),
                   trControl = ctrl
                   )

# Visualization of importance variables of the various models

rf_model |> 
  varImp() |> 
 plot(main = "Random Forest variable importance")

lda_model |> 
  varImp() |> 
  plot(main = "KNN, Linear Discrminant Analysis and Support Vector Machine variable Importance.")

# Evaluating the performance and accuracy of the models 

# 1. Random Forest

rf_pred <- predict(rf_model, newdata = test_iris)
confusionMatrix(data = rf_pred,
                reference = test_iris$Species)


# 2. KNN

knn_pred <- predict(knn_model, newdata = test_iris)
confusionMatrix(data = knn_pred, 
                reference = test_iris$Species)

# 3. Linear Discriminant Analysis

lda_pred <- predict(lda_model, newdata = test_iris)
confusionMatrix(data = lda_pred, 
                reference = test_iris$Species)

# 4. Support Vector Machine

svm_pred <- predict(svm_model, test_iris)
confusionMatrix(data = svm_pred, 
                reference = test_iris$Species)

# Predicting the Species of the Iris flower with new set of data

newdata = list(Sepal.Length = 5.1, 
               Sepal.Width = 3.5, 
               Petal.Length = 1.4, 
               Petal.Width = 0.2)

predict(knn_model, newdata)
predict(lda_model, newdata)
predict(svm_model, newdata)
predict(rf_model, newdata)

names(getModelInfo(
  
))


?preProcess
?caret
library(caret)
