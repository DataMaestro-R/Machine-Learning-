library(missMDA)
library(tidyverse)
library(caret)
library(mice)

theme_set(new = theme_light())

data("snorena")
snorena

snorena %>% 
  is.na() %>% 
  sum()

VIM::aggr(snorena, col = c("navyblue", 
                           "green"))

snorena %>% 
  summary()

snorena %>% 
  glimpse()

proc <- snorena %>% 
  preProcess(method = "bagImpute")
snorena_update <- predict(proc, snorena)



airquality %>% 
  glimpse()


airquality <- airquality %>% 
  mutate(Day = as.factor(Day))

VIM::aggr(airquality,
          col = c("orange", 
                  "blue"))


air_proc <- airquality %>% 
  preProcess(method = "bagImpute")
air <- air_proc %>% 
  predict(airquality)


t.test(air$Ozone, airquality$Ozone)



# Install and load the openxlsx package
install.packages("openxlsx")
library(openxlsx)

# Use the mtcars dataset as an example
data <- snorena

# Specify the file path where you want to save the Excel file
file_path <- "G:/Project_Falcon"

# Write the data to an Excel file
write.xlsx(data, file_path, sheetName = "Sheet1", rowNames = FALSE)
write.csv(data, file_path, row.names = FALSE)
