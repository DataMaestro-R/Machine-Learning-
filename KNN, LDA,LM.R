library(tidyverse)
library(caTools)
library(caret)
library(class)
library(ROCR)


# ML KNN

# Importation of the wine data

wine <- read.csv(choose.files())
wine %>% 
  view()
wine <- wine %>% 
  dplyr::select(Country, Rating, Price, Alcohol, Residual_Sugar, Sulphates) %>% 
  mutate(Country = factor(Country))

wine %>% 
  summary() 

wine_summary <- psych::describe(wine[, -1])
wine_summary <- wine_summary %>% 
  dplyr:: select(vars, mean, sd, median, min, max, range, se)


wine_pct <- wine %>% 
  group_by(Country) %>% 
  summarise(Frequency = n(),
            Percentage = (Frequency/nrow(wine))*100)
wine_pct %>% 
  print()


wine %>% 
  ggplot() +
  geom_bar(aes(x = Country, fill = Country)) +
  scale_fill_manual(values = c("red", "navy", "gold", "green")) + theme(plot.caption = element_text(family = "serif",
    size = 15, face = "bold.italic"), axis.ticks = element_line(linetype = "blank"),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    axis.title = element_text(family = "serif",
        size = 15, face = "bold.italic"),
    axis.text = element_text(family = "serif",
        size = 15, face = "bold.italic"),
    plot.title = element_text(family = "serif",
        size = 20, face = "bold.italic",
        hjust = 0.5)) +labs(title = "Wine Distribution of the Various Countries",
    y = "Frequency", caption = "Source : Wine dataset (Git_hub/DataMaestro)") + theme(plot.subtitle = element_text(family = "serif",
    size = 15, face = "italic", hjust = 0.5),
    legend.text = element_text(face = "bold.italic",
        family = "serif"), legend.title = element_text(face = "bold.italic",
        family = "serif"), legend.background = element_rect(fill = NA),
    legend.position = "top") +labs(subtitle = "Countries: Canada, France, Italy, and US.",
    caption = "Source : Wine dataset (Git_hub/DataMaestro).") + theme(legend.text = element_text(size = 15),
    legend.title = element_text(size = 15))
  



# Normalize function

normalize <- function(x){
  results <- ((x-min(x)) / (max(x) - min(x)))
  return(results)
}


wine_subset <- wine[, -1] %>% 
  lapply(normalize) %>% 
  data.frame()

# Partitioning the dataset into training and test data.

set.seed(90)

Index <- sample(1:nrow(wine_subset, 
                       size = (nrow(wine_subset)*0.7), 
                       replace = FALSE))


train_data <- wine_subset[Index, ]
test_data <- wine_subset[-Index, ]


train_data_label <- wine[Index, 1]
test_data_label <- wine[-Index, 1]

# Using the KNN Approach

knn_wine <- knn(train = train_data, test = test_data, 
                cl = train_data_label, k = 11)

# Calculate the perfomance of correct classification

table(test_data_label, knn_wine) %>% 
  confusionMatrix()


# A for loop for the various values of K for optimal accuracy

i = 1
k.optm = 1

for (i in 1: length(test_data_label)) {
  
  knn.mod <- knn(train = train_data, test = test_data, 
                 cl = train_data_label, k = i)
  
  
  k.optm[i] <- mean(test_data_label == knn.mod)*100
  
  k = i
  cat(k, '=', k.optm[i], '\n')
}

# Accuracy plot

plot(k.optm, type = "b", xlab = "K-value", ylab = "Accuracy level", col = "blue")


new = list(Rating = 54.5, Price = 53, Alcohol = 9.3, Residual_Sugar = 2.3, Sulphates = 0.9)



lda.model <- lda(Country ~ ., data = wine, scale = T)

predict(lda.model, newdata = new)$class
