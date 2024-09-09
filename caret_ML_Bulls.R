library(tidyverse)
library(caret)
library(caretEnsemble)
library(caretForecast)
library(bestNormalize)


Bulls <- choose.files() |> 
  read.table()

Bulls |> 
  glimpse()


Bulls <- Bulls |> 
  rename(Breed = V1, SalePr = V2, YrHgt = V3, FtFrBody = V4, PrctFFB = V5,
         Frame = V6, BkFat = V7, SaleHt = V8, SaleWt = V9) |> 
  mutate(Breed = case_when(Breed == 1 ~ "Angus",
                           Breed == 5 ~ "Hereford",
                           Breed == 8 ~ "Simental")) |> 
  mutate(across(.col = Breed,
                .fns = factor))

Bulls |> 
  str()


set.seed(13)

Bdex <- createDataPartition(Bulls$Breed,
                            p = 0.7,
                            list = FALSE)

train.Bulls <- Bulls[Bdex, ]
test.Bulls <- Bulls[-Bdex, ]


Bulls.trnf <- preProcess(train.Bulls, 
                         method = c("center", 
                                    "scale"))

train.Bulls <- predict(Bulls.trnf, train.Bulls)


Bulls.trmf <- preProcess(test.Bulls, 
                         method = c("center",
                                    "scale"))

test.Bulls <- predict(Bulls.trmf, test.Bulls)


trC <- trainControl(method = "repeatedcv",
                    number = 10, 
                    repeats = 3)

set.seed(23)
Bull.lda <- train(Breed ~ ., 
                  data = train.Bulls,
                  trControl = trC,
                  method = "lda")



pred.Bulls <- predict(Bull.lda, test.Bulls)
confusionMatrix(data = pred.Bulls,
                reference = test.Bulls$Breed)
