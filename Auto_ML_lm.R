library(tidyverse)
library(caret)
library(caretEnsemble)
library(bestNormalize)


auto <- read.csv("G:/Project_Falcon/r regression plot/2021-07-09 r regression plot/Auto.csv")
auto 


auto |> glimpse()

auto <- auto |> 
  select(origin, acceleration, weight, horsepower, displacement, 
         cylinders, mpg) |> 
  mutate(across(.cols = origin,
                .fns = factor))



auto_trans <- preProcess(auto, method = "YeoJohnson")
auto <- predict(auto_trans, newdata = auto)

lm <- lm(weight ~ acceleration + cylinders + horsepower + displacement +
           mpg, data = auto)



lm_caret <- train(displacement ~ acceleration + cylinders + horsepower + weight +
                    mpg,
                  data = auto,
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5,
                                           allowParallel = TRUE),
                  method = "lm")


data <- list(mpg = 18, 
            cylinders = 8,
            displacement = 307,
            horsepower = 130,
            acceleration = 12)

predict(lm, data)
