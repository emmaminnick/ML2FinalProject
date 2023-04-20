library(tidyverse)
library(FNN)
library(randomForest)

data <- read.csv("train.csv")


# Data Prep / Test and Train Split ----------------------------------------

df <- data %>% 
  filter(between(x, 4.5, 4.75),
         between(y, 4.5, 4.75)) %>% 
  group_by(place_id) %>% 
  mutate(Count = n()) %>% 
  filter(Count > 15) %>% 
  select(!row_id)

df <- na.omit(df)

trainIndex = sample(1:nrow(df), 0.8 * nrow(df))

head(df)
summary(df)

train.x <- df[trainIndex, -5]
train.x <- scale(train.x)

train.y <- df$place_id[trainIndex]

test.x <- df[-trainIndex, -5]
test.x <- scale(test.x)

test.y <- df$place_id[-trainIndex]

# Exploratory Data Analysis -----------------------------------------------

summary(data)

subset_df <- df %>% 
  filter(between(x, 4.5, 4.75),
         between(y, 4.5, 4.75))

ggplot(subset_df, aes(x = x, y = y))+
  geom_point(aes(size = 1/accuracy, color = place_id))

#Accuracy is how confident we are, so graphing 1/Accuracy allows us to see where empty spaces are

ggplot(subset_df, aes(x = time, y = place_id))+
  geom_point(aes(size = Count))

#Can see that some places are visited differently over the course of the day and some are constantly busy

# Bagging -----------------------------------------------------------

model_RF <- randomForest(place_id ~ .,
                         data=df,
                         subset=trainIndex,
                         mtry=5,
                         importance = TRUE)

yhat<-predict(model_RF,
              newdata=df[-trainIndex,])

mean(df$place_id[-trainIndex] == yhat)

#Accuracy is alright for our Bagging Model at about 43%

varImpPlot(model_RF)

#The most important feature is the Count of people that are at the same place


# KNN Clustering -----------------------------------------------------

Accuracy <- rep(0, 30)

for (i in 1:30){
model_knn = FNN::knn(train = train.x, test = test.x, cl = train.y, k = i)

Accuracy[i] = mean(test.y == model_knn)
}

which.max(Accuracy)

Accuracy

#Best is ~ 5 nearest Neighbors, though most are similar at ~ 72-73%

