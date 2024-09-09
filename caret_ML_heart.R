library(tidyverse)
library(caret)
library(caretEnsemble)
library(caretForecast)
library(readxl)
library(MASS)
library(mlbench)


heart <- read.csv("G:/Project_Falcon/Kaggle_data/heart_disease_health_indicators_BRFSS2015.csv")
heart |> glimpse()
heart |> names()
heart |> is.na()


heart <- heart |> 
  dplyr:: mutate(across(.cols = !c(BMI, Age, HeartDiseaseorAttack),
                        .fns = factor))

heart$HeartDiseaseorAttack <- factor(heart$HeartDiseaseorAttack,
                                        levels = c(0, 1),
                                      label = c("HeartDisease", "HeartAttack"))


heart |> 
  summary()


heart |> 
  ggplot()+
  geom_bar(aes(Income, fill = Income))+ 
  geom_col(color = "black")


heart_bar <- heart |> 
  group_by(Education) |> 
  summarise(Frequency = n(),
            percentage = ((Frequency/nrow(heart))*100) |> 
              round(2))



heart_bar |> 
  ggplot(aes(Education, Frequency, fill = Education)) +
  geom_bar(stat = "identity") + 
  geom_col(color = "black") +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5),
            size = 7)



hdex <- createDataPartition(heart$HeartDiseaseorAttack,
                            p = 0.7, 
                            list = FALSE)

heart_train <- heart[hdex, ]
heart_test <- heart[-hdex, ]


prop.table(table(heart$HeartDiseaseorAttack))
prop.table(table(heart_train$HeartDiseaseorAttack))
prop.table(table(heart_test$HeartDiseaseorAttack))



m <- dummyVars(HeartDiseaseorAttack ~ ., data = heart_train)
heart_trainx <- as.data.frame(predict(m, heart_train))


heart_trainn <- cbind(heart_train$HeartDiseaseorAttack, heart_trainx)
names(heart_trainn)[1] <- "HeartDiseaseorAttack"


n <- dummyVars(HeartDiseaseorAttack ~ ., data = heart_test)
hear_testx <- as.data.frame(predict(n, heart_test))

heart_testt <- cbind(heart_test$HeartDiseaseorAttack, hear_testx)
names(heart_testt)[1] <- "HeartDiseaseorAttack"



control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        classProbs = TRUE,
                        allowParallel = FALSE)



logit <- train(HeartDiseaseorAttack ~ .,
               data = heart_trainn,
               method = "glm",
               family = "binomial",
               metric = "ROC",
               trControl = control,
               preProc = c("center", "scale"))


heart_pred <- predict(logit, heart_testt)
confusionMatrix(data = heart_pred,
                reference = heart_test$HeartDiseaseorAttack)
